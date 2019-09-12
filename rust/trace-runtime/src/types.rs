use bumpalo::Bump;
use futures::{future, Async, Future, Poll};
use std::{marker::PhantomData, pin::Pin};

/// An empty type. We need to define our own until the `!` type is
/// stabilized. See the [RFC 1216] for more information.
///
/// [RFC 1216]: https://github.com/rust-lang/rfcs/blob/master/text/1216-bang-type.md
pub enum Never {}

/// This is a *family trait*. See  this [blog post] for more information.
///
/// [blog post]: http://lukaskalbertodt.github.io/2018/08/03/solving-the-generalized-streaming-iterator-problem-without-gats.html
pub trait FamilyLt<'a> {
    type Out;
}

/// An enumeration of the events that a decontainerized function can run
/// within Rust.
pub enum AsyncOp {
    /// This event immediately completes, which is useful for testing without
    /// performing real I/O.
    Immediate,
    Request(String),
}

/// The execution context allows a callback to send new events.
pub struct ExecutionContext {
    new_ops: Vec<(AsyncOp, usize)>,
}

impl ExecutionContext {
    /// Send a new event. The argument `indicator` is sent back with the
    /// response, which helps the decontainerized keep track of multiple
    /// pending requests. There is no requirement that indicators be distinct,
    /// but that will be helpful to client code.
    pub fn loopback(&mut self, op: AsyncOp, indicator: usize) {
        self.new_ops.push((op, indicator));
    }
}

/// All decontainerized functions implement this trait.
///
/// Every decontainerized function implements a pair of Rust functions
/// called `new` and `callback`. The `new` function returns the initial state
/// and `callback` takes the current state as an argument and an execution
/// context. The execution context has a method called `loopback` that allows
/// `callback` to send events and run again (i.e., loopback) when a response
/// to the event is received. If `callback` returns without sending new events
/// and there are no pending events, then the decontainerized function completes
/// successfully.
///
/// Finally, to facilitate memory allocation, `new` receives a borrowed
/// reference to an arena. Therefore, the state produced by `new` contains
/// references to the arena.
///
///  The natural definition of the trait is as follows:
///
/// ```text
/// pub trait Decontainerized {
///     type State;
///     fn new(arena: &Bump) -> State
///     fn callback(arena: &Bump, state: &State, &mut ExecutionContext) -> ();
/// }
/// ```
///
/// Although this code compiles, this definition does not work because
/// `State` to contain borrowed pointers with the same lifetime as the arena.
/// We do not want to add a lifetime parameter to the trait itself, since it
/// will "bubble up" to the rest of the runtime system. Ideally, we would
/// be able to write the following type:
///
/// ```text
/// pub trait Decontainerized {
///     type State<'x>;
///     fn new<'a>(arena: &'a Bump) -> State<'a>
///     fn callback<'a,'b>(
///         arena: &'a Bump,
///         state: &'a State,
///         ec: &'mut ExecutionContext)
///         -> ();
/// }
/// ```
///
/// However, the `type State<'x>` notation is a proposed Rust extension that
/// has not be implemented (even in nightly). As a workaround, we use
/// type families as shown below.
pub trait Decontainerized {
    /// Note that the methods `new` and `callback` require this associated type
    /// to implement the trait `FamilyLt<'a>`. A typical implementation will be
    /// as follows:
    ///
    /// ```ignore
    /// struct Family(Never);
    ///
    /// impl<'a> FamilyLt<'a> for Family {
    ///     type Out = State<'a>;
    /// }
    /// ```
    ///
    /// In the code above, `State<'a>` is a structure that holds the global
    /// state of the decontainerized function. Moreover, the structure is
    /// parameterized by the lifetime `'a`, which is the lifetime of the
    /// arena allocator. Therefore, the structure can hold borrowed references
    /// to values allocated in the arena.
    type StateFamily: Send;

    /// Initializes the decontainerized function. This function borrows the
    /// arena and produces a structure with the type `StateFamily<'a>::Out`.
    /// That structure may be parameterized by `'a,`, thus may hold references
    /// to values allocated in `arena`.
    fn new<'a>(arena: &'a Bump) -> <Self::StateFamily as FamilyLt<'a>>::Out
    where
        Self::StateFamily: FamilyLt<'a>;

    fn callback<'a, 'b>(
        arena: &'a Bump,
        state: &'a mut <Self::StateFamily as FamilyLt<'a>>::Out,
        ec: &'b mut ExecutionContext,
    ) -> ()
    where
        Self::StateFamily: FamilyLt<'a>;
}

// This structure owns an arena and has an unsafe implementation of `Send`.
// In general, it is not safe to send `Bump` across threads without a mutex.
// However, the `poll` method in `Decontainer` ensures that only one thread
// accesses `bump` at a time, which is why this is safe.
struct Arena {
    bump: Bump,
}

unsafe impl Send for Arena {}

// TODO(arjun): Does this need to have a `PhantomPin` field?
struct DecontainerImpl<T>
where
    T: Send + Sized + Decontainerized,
{
    task: PhantomData<T>,
    arena: Arena,
    machine_state: Vec<(bool, Box<Future<Item = usize, Error = ()> + Send>)>,
    // We really mean for `state` to have the following type:
    //
    // ```
    // Box<<<T as Decontainerized>::StateFamily as FamilyLt<'a>>::Out>
    // ```
    //
    // i.e., it should be a boxed state-structure, which may hold borrowed
    // references to the arena. This is essentially impossible to do in
    // safe Rust, since both `task` and the borrowed references need to be
    // members of the same struct. As a workaround, we cast the contents of
    // the box from its actual type to the type `Never`. Since the `Never`
    // type has no constructors, we won't accidentally use it.  The function
    // ``remember_type` below makes it hard to cast the `Never` to the
    // wrong type.
    state: Box<Never>,
}

unsafe fn remember_type<'a, T>(
    state: &'a mut Box<Never>,
) -> &'a mut Box<<<T as Decontainerized>::StateFamily as FamilyLt<'a>>::Out>
where
    T: Decontainerized,
    <T as Decontainerized>::StateFamily: FamilyLt<'a>,
{
    return std::mem::transmute(state);
}

fn forget_type<T>(x: Box<T>) -> Box<Never> {
    unsafe {
        return std::mem::transmute(x);
    }
}

pub struct Decontainer<T>
where
    T: Send + Sized + Decontainerized,
{
    pinned: Pin<Box<DecontainerImpl<T>>>,
    ec: ExecutionContext,
}

#[allow(unused)]
impl<T> Decontainer<T>
where
    T: Send + Sized + Decontainerized,
    <T as Decontainerized>::StateFamily: for<'a> FamilyLt<'a>,
{
    /// Creates a new instance of a decontainerized function.
    ///
    /// This function allocates an arena and calls `T::new` with a reference
    /// to that arena. Since this function does not take any parameters,
    /// the type `T` must be determined by the calling context. E.g.,
    ///
    /// ```ignore
    /// Decontainer::new() as Decontainer<ExampleTask>
    /// ```
    pub fn new() -> Decontainer<T> {
        let arena = Bump::new();
        let state = forget_type(Box::new(T::new(&arena)));
        return Decontainer {
            pinned: Box::pin(DecontainerImpl {
                arena: Arena { bump: arena },
                state: state,
                task: PhantomData,
                machine_state: vec![(false, Box::new(future::ok(0)))],
            }),
            ec: ExecutionContext {
                new_ops: Vec::new(),
            },
        };
    }
}

#[allow(unused)]
impl<T> Future for Decontainer<T>
where
    T: Send + Sized + Decontainerized,
    <T as Decontainerized>::StateFamily: for<'a> FamilyLt<'a>,
{
    type Item = ();
    type Error = ();

    fn poll<'a>(&'a mut self) -> Poll<Self::Item, Self::Error> {
        // Boilerplate to address pinning
        let mut_ref = unsafe { Pin::as_mut(&mut self.pinned) };
        let self_ = unsafe { Pin::get_unchecked_mut(mut_ref) };

        assert!(self.ec.new_ops.len() == 0);
        let mut ec = &mut self.ec;
        loop {
            // Nothing left to do, so we are ready. Note that
            // Decontainer::new() adds a single future to machine_state, so
            // this is not empty initially.
            if self_.machine_state.len() == 0 {
                return Result::Ok(Async::Ready(()));
            }
            let mut any_completed = false;
            for (completed, future) in self_.machine_state.iter_mut() {
                assert!(*completed == false);
                match future.poll() {
                    Result::Err(err) => {
                        eprintln!("Error: {:?}", &err);
                        return Result::Err(err);
                    }
                    Result::Ok(Async::NotReady) => {
                        // Try next future
                    }
                    Result::Ok(Async::Ready(n)) => {
                        *completed = true;
                        any_completed = true;
                        let state = unsafe { remember_type::<T>(&mut self_.state) };
                        T::callback(&self_.arena.bump, state, &mut ec);
                    }
                }
            }
            // Nothing completed, thus never executed T::callback.
            if any_completed == false {
                return Result::Ok(Async::NotReady);
            }
            self_
                .machine_state
                .retain(|(completed, _)| *completed == false);
            // Create a future for each new operation.
            for (op, indicator) in ec.new_ops.iter() {
                match op {
                    AsyncOp::Immediate => {
                        self_
                            .machine_state
                            .push((false, Box::new(future::ok(*indicator))));
                    }
                    AsyncOp::Request(_) => {
                        panic!("request not yet implemented");
                    }
                }
            }
            ec.new_ops.clear();
        }
    }
}

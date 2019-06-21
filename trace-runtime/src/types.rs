use std::{pin::Pin, marker::PhantomData};
use futures::{Future, Poll, Async};
use bumpalo::Bump;

/// An empty type. We need to define our own until the `!` type is
/// stabilized. See the [RFC 1216] for more information.
///
/// [RFC 1216]: https://github.com/rust-lang/rfcs/blob/master/text/1216-bang-type.md
pub enum Never { }

/// This is a *family trait*. See  this [blog post] for more information.
///
/// [blog post]: http://lukaskalbertodt.github.io/2018/08/03/solving-the-generalized-streaming-iterator-problem-without-gats.html
pub trait FamilyLt<'a> {
    type Out;
}

/// All decontainerized functions implement this trait.
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
    type StateFamily : Send;

    /// The type result produced by the decontainerized function. It is possible
    /// for this type to be a particular structure that implements `Future`.
    /// However, a simple alternative is the following type, which will work
    /// for any function by boxing the result:
    ///
    /// ```ignore
    /// type Result = Box<Future<Item = (), Error = ()> + Send>;
    /// ```
    ///
    /// We should not worry about a single extra box for every request.
    type Result : Future<Item = (), Error = ()> + Send;

    /// Initializes the decontainerized function. This function borrows the
    /// arena and produces a structure with the type `StateFamily<'a>::Out`.
    /// That structure may be parameterized by `'a,`, thus may hold references
    /// to values allocated in `arena`.
    fn new<'a>(
        arena: &'a Bump) ->
        <Self::StateFamily as FamilyLt<'a>>::Out where
        Self::StateFamily: FamilyLt<'a>;

    fn callback<'a>(
        arena: &'a Bump,
        state: &'a mut <Self::StateFamily as FamilyLt<'a>>::Out)
        -> Option<Self::Result> where
      Self::StateFamily: FamilyLt<'a>;

}

enum MachineState<T> {
    Ready,
    Waiting(T),
    Done
}

// This structure owns an arena and has an unsafe implementation of `Send`.
// In general, it is not safe to send `Bump` across threads without a mutex.
// However, the `poll` method in `Decontainer` ensures that only one thread
// accesses `bump` at a time, which is why this is safe.
struct Arena {
    bump: Bump
}

unsafe impl Send for Arena { }

// TODO(arjun): Does this need to have a `PhantomPin` field?
struct DecontainerImpl<T> where
  T : Send + Sized + Decontainerized {
    task: PhantomData<T>,
    arena: Arena,
    machine_state: MachineState<T::Result>,
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
    state: Box<Never>
}

unsafe fn remember_type<'a, T>(state: &'a mut Box<Never>)
-> &'a mut Box<<<T as Decontainerized>::StateFamily as FamilyLt<'a>>::Out> where
  T : Decontainerized,
  <T as Decontainerized>::StateFamily: FamilyLt<'a> {
      return std::mem::transmute(state);
}

fn forget_type<T>(x: Box<T>) -> Box<Never> {
    unsafe {
        return std::mem::transmute(x);
    }
}

pub struct Decontainer<T> where
    T : Send + Sized + Decontainerized {
    pinned: Pin<Box<DecontainerImpl<T>>>
}

#[allow(unused)]
impl<T> Decontainer<T> where
    T : Send + Sized + Decontainerized,
    <T as Decontainerized>::StateFamily: for<'a> FamilyLt<'a> {

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
                machine_state: MachineState::Ready
            })
        };
    }

}

#[allow(unused)]
impl<T> Future for Decontainer<T> where
    T : Send + Sized + Decontainerized,
    <T as Decontainerized>::StateFamily: for<'a> FamilyLt<'a>,
    <T as Decontainerized>::Result: futures::future::Future  {

    type Item = ();
    type Error = ();

    fn poll<'a>(
        &'a mut self
    ) -> Poll<Self::Item, Self::Error> {
        let mut_ref = unsafe { Pin::as_mut(&mut self.pinned) };
        let self_ = unsafe { Pin::get_unchecked_mut(mut_ref) };
        loop {
            match &mut self_.machine_state {
                MachineState::Done => panic!("already done"),
                MachineState::Ready => {
                    let state = unsafe { remember_type::<T>(&mut self_.state) };
                    match T::callback(&self_.arena.bump, state) {
                        Option::None => {
                            self_.machine_state = MachineState::Done;
                            return Result::Ok(Async::Ready(()));
                        },
                        Option::Some(f) => {
                            self_.machine_state = MachineState::Waiting(f)
                        }
                    }
                }
                MachineState::Waiting(future) => {
                    match future.poll() {
                        Result::Err(err) => {
                            eprintln!("Error: {:?}", &err);
                            return Result::Err(err)
                        },
                        Result::Ok(Async::NotReady) => {
                            return Result::Ok(Async::NotReady);
                        },
                        Result::Ok(Async::Ready(_)) => {
                            self_.machine_state = MachineState::Ready;
                        }
                    }
                }
            }
        }
    }

}

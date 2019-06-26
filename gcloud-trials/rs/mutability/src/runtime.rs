//! The runtime system for decontainerized serverless functions.
//!
//! The `hyper` crate produces futures that requires their items to implement
//! `Send`. Therefore, we cannot share state using `RefCell`. For example,
//! the following program does not compile:
//!
//! ```no_run
//! use hyper::Client;
//! use hyper::rt::{self, Future};
//! use std::rc::Rc;
//! use std::cell::RefCell;
//!
//! fn main() {
//!     let client = Client::new();
//!     let state = Rc::new(RefCell::new(0));
//!     rt::run(client
//!         .get("http://www.umass.edu".parse::<hyper::Uri>().unwrap())
//!         .map(|_| {
//!             *state.borrow_mut() = 10;
//!        })
//!         .map_err(|err| panic!("{:?}", err)));
//! }
//! ```
//!
//! This is a legitimate error, because it is possible for a program to use
//! rt::spawn to launch two parallel tasks that both mutate the `state`.
//!
//! An alternative that does compile uses `Arc` and `Mutex` to guard the
//! shared state:
//!
//! ```no_run
//! use hyper::Client;
//! use hyper::rt::{self, Future};
//! use std::sync::{Arc, Mutex};
//!
//! fn main() {
//!     let client = Client::new();
//!     let state = Arc::new(Mutex::new(0));
//!     rt::run(client
//!         .get("http://www.umass.edu".parse::<hyper::Uri>().unwrap())
//!         .map(move |_| {
//!             *state.lock().unwrap() = 10;
//!        })
//!         .map_err(|err| panic!("{:?}", err)));
//! }
//!```
//!
//! However, this approach will have high overhead due to the need for a mutex
//! for every piece of shared state.

#![allow(unused_assignments)]

use hyper::Client;
use futures::{Future, Poll, Async};
use bumpalo::Bump;
use futures::sync::oneshot;
#[macro_use]
use futures::*;
use std::marker::PhantomData;

pub type Callback = Box<dyn for <'a> FnMut(&'a mut ExecutionContext) -> ()>;

pub trait FamilyLt<'a> {
    type Out;
}

enum Never { }

pub trait Decontainerized {

    type StateFamily : Send;
    type Result : Future<Item = (), Error = ()> + Send;
    fn new<'a>(
        arena: &'a Bump) ->
        <Self::StateFamily as FamilyLt<'a>>::Out where
        Self::StateFamily: FamilyLt<'a>;

    fn callback<'a>(
        arena: &'a Bump,
        state: &'a mut <Self::StateFamily as FamilyLt<'a>>::Out)
        -> Option<Self::Result> where
      Self::StateFamily: FamilyLt<'a>,
      Self::Result : Future<Item = (), Error = ()>;

}

pub struct ExampleState<'a> {
    x: &'a i32
}

pub struct ExampleStateFamily(Never);

impl ExampleStateFamily {
    fn from<'a>(x: ExampleState<'a>) -> <Self as FamilyLt<'a>>::Out {
        x
    }
    fn to<'a>(x: &'a mut <Self as FamilyLt<'a>>::Out) -> &'a mut ExampleState<'a> {
        x
    }
}

impl<'a> FamilyLt<'a> for ExampleStateFamily {
    type Out = ExampleState<'a>;
}

pub enum ExampleTask { }

impl Decontainerized for ExampleTask {
    type StateFamily = ExampleStateFamily;
    type Result = Box<Future<Item = (), Error = ()> + Send>;
    fn new<'a>(
        arena: &'a Bump) ->
        <Self::StateFamily as FamilyLt<'a>>::Out where
      Self::StateFamily: FamilyLt<'a> {
        let y = arena.alloc(100);
        ExampleStateFamily::from(ExampleState { x: y })
    }

    fn callback<'a>(
        arena: &'a Bump,
        state: &'a mut <Self::StateFamily as FamilyLt<'a>>::Out)
    -> Option<Self::Result> where
      Self::StateFamily: FamilyLt<'a> {
        let state = ExampleStateFamily::to(state);
        println!("{}", state.x);
        let y = arena.alloc(*state.x + 1);
        state.x = y;
        if *state.x == 150 {
            return Option::None;
        }
        return Option::Some(Box::new(future::ok(())));
    }

}

enum MachineState<T> {
    Ready,
    Waiting(T),
    Done
}

pub struct Decontainer<T> where
  T : Send + Sized + Decontainerized {
    task: PhantomData<T>,
    arena: Bump,
    machine_state : MachineState<T::Result>,
    state: Box<Never>
}

unsafe impl<T> Send for Decontainer<T> where
T : Send + Sized + Decontainerized { }

unsafe fn remember_type<'a, T>(state: &'a mut Box<Never>) -> &'a mut Box<<<T as Decontainerized>::StateFamily as FamilyLt<'a>>::Out> where
  T : Decontainerized,
  <T as Decontainerized>::StateFamily: FamilyLt<'a> {
      return std::mem::transmute(state);
}

fn forget_type<T>(x: Box<T>) -> Box<Never> {
    unsafe {
        return std::mem::transmute(x);
    }
}


impl<T> Decontainer<T> where
    T : Send + Sized + Decontainerized,
    <T as Decontainerized>::StateFamily: for<'a> FamilyLt<'a> {

    // pub fn run<'a>(&'a mut self) -> () {
    //     let mut state = unsafe { remember_type::<'a, T>(&mut self.state) };
    //     T::callback(&self.arena, state);
    // }

    pub fn new() -> Decontainer<T> {
        let arena = Bump::new();
        let state = forget_type(Box::new(T::new(&arena)));
        return Decontainer {
            arena: arena,
            state: state,
            task: PhantomData,
            machine_state: MachineState::Ready
        };
    }

}

impl<T> Future for Decontainer<T> where
    T : Send + Sized + Decontainerized,
    <T as Decontainerized>::StateFamily: for<'a> FamilyLt<'a>,
    <T as Decontainerized>::Result: futures::future::Future  {

    type Item = ();
    type Error = ();

    fn poll<'a>(
        &'a mut self
    ) -> Poll<Self::Item, Self::Error> {
        loop {
            match &mut self.machine_state {
                MachineState::Done => panic!("already done"),
                MachineState::Ready => {
                    let state = unsafe { remember_type::<T>(&mut self.state) };
                    println!("Running callback");
                    match T::callback(&self.arena, state) {
                        Option::None => {
                            self.machine_state = MachineState::Done;
                            return Result::Ok(Async::Ready(()));
                        },
                        Option::Some(f) => {
                            self.machine_state = MachineState::Waiting(f)
                        }
                    }
                }
                MachineState::Waiting(future) => {
                    match future.poll() {
                        Result::Err(err) => return Result::Err(err),
                        Result::Ok(Async::NotReady) => return Result::Ok(Async::NotReady),
                        Result::Ok(Async::Ready(_)) => self.machine_state = MachineState::Ready
                    }
                }
            }
        }
    }

}



struct MiniTask {
    callback : Box<for <'a> FnMut(&'a mut ExecutionContext) -> ()>,
    future: Box<Future<Item = (), Error = ()> + Send>,
    completed: bool
}

pub struct Runtime {
    tasks: Vec<MiniTask>,
    sender: Option<oneshot::Sender<()>>,
    ec: ExecutionContext
}

pub struct ExecutionContext {
    new_tasks: Vec<MiniTask>
}

/// Creates a new runtime for a program that starts with the given main
/// function.
///
/// 
pub fn run(main: Callback)
    -> impl Future<Item = (), Error = ()> {
    let (sender, receiver) = oneshot::channel::<()>();
    let main_task = MiniTask {
        callback: main,
        future: Box::new(futures::done(Ok(()))),
        completed: false
    };
    let runtime = Runtime {
        tasks: vec! [main_task],
        sender: Option::Some(sender),
        ec: ExecutionContext {
            new_tasks: Vec::new()
        }
    };
    futures::executor::spawn(runtime);
    return receiver
        .map_err(|_| ());
}


    /*
     * Creates a new runtime for a program that starts at main. The program
     * receives an 'ExecutionContext' as an argument, which it can use to
     * perform asynchronous I/O and execute callbacks when I/O completes.
     * However, the runtime ensures that callbacks never execute in parallel.
     */


impl Future for Runtime {
    type Item = ();
    type Error = ();
    fn poll(
        &mut self
    ) -> Poll<Self::Item, Self::Error> {
        loop {
            if self.tasks.is_empty() {
                let mut sender = Option::None;
                std::mem::swap(&mut self.sender, &mut sender);
                sender.unwrap().send(())
                  .expect("Failed to send");
                return Result::Ok(Async::Ready(()));
            }

            let mut error : Option<()> = Option::None;


            let mut any_ready = false;
            for task in self.tasks.iter_mut() {
                assert_eq!(task.completed, false);
                match task.future.poll() {
                    Result::Err(err) => {
                        eprintln!("ERROR");
                        error = Option::Some(err)
                    },
                    Result::Ok(Async::NotReady) => (),
                    Result::Ok(Async::Ready(_)) => {
                        let f = &mut task.callback;
                        task.completed = true;
                        f(&mut self.ec);
                        any_ready = true;
                    }
                }
            }

            self.tasks.append(&mut self.ec.new_tasks);
            if any_ready == false {
                return Result::Ok(Async::NotReady);
            }
            self.tasks.retain(|task| !task.completed);
        }
    }
}
impl ExecutionContext {

    /**
     * Make a request to a URI and register a callback to execute when the
     * request completes. The callback will _not_ run in parallel with any
     * other callbacks or the main program.
     */
    pub fn request<'a>(
        &'a mut self,
        uri: &str,
        callback: Box<FnMut(&'a mut ExecutionContext) -> ()>) {
        eprintln!("request({})", uri);
        let uri2 = uri.to_owned();
        let client = Client::new();
        let resp = client
            .get(uri.parse::<hyper::Uri>().unwrap())
            .and_then(move |res| {
                eprintln!("Got response from {}", uri2);
                futures::done(Result::Ok(()))
            })
            .map_err(|err| {
                eprintln!("Error {}", err);
            });
        // let mini_task = MiniTask {
        //     callback : Box::new(callback),
        //     future : Box::new(resp),
        //     completed: false
        // };
        // self.new_tasks.push(mini_task);
    }

}

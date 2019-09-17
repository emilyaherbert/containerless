use bumpalo::Bump;
use futures::{future, Async, Future, Poll};
use std::{pin::Pin};
use crate::execution_context::*;
use super::type_dynamic::{Dyn, DynResult};

// This structure owns an arena and has an unsafe implementation of `Send`.
// In general, it is not safe to send `Bump` across threads without a mutex.
// However, the `poll` method in `Decontainer` ensures that only one thread
// accesses `bump` at a time, which is why this is safe.
unsafe impl Send for DecontainerImpl { }

// TODO(arjun): Does this need to have a `PhantomPin` field?
struct DecontainerImpl {
    arena: Bump,
    machine_state: Vec<(bool, Dyn<'static>, Box<dyn Future<Item = usize, Error = ()> + Send>)>,
    ec: ExecutionContext<'static>,
    func: CB
}


pub struct Decontainer {
    pinned: Pin<Box<DecontainerImpl>>
}

type CB = Box<dyn for <'a, 'b> Fn(&'a Bump, &'b mut ExecutionContext<'a>, Dyn<'a>, Dyn<'a>) -> DynResult<'a>>;

impl Decontainer {
    pub fn new(f: CB) -> Decontainer {
        let arena = Bump::new();
        return Decontainer {
            pinned: Box::pin(DecontainerImpl {
                arena: arena,
                machine_state: vec![(false, Dyn::Int(0), Box::new(future::ok(0)))],
                ec: ExecutionContext {
                   new_ops: Vec::new(),
                },
                func: f
            })
        };
    }
}

unsafe fn shorten_invariant_lifetime<'b, 'c>(r: &'b mut ExecutionContext<'static>)
                                             -> &'b mut ExecutionContext<'c> {
    std::mem::transmute::<&'b mut ExecutionContext<'static>, &'b mut ExecutionContext<'c>>(r)
}

unsafe fn shorten_invariant_lifetime2<'a>(v: Dyn<'static>) -> Dyn<'a> {
    std::mem::transmute(v)
}

unsafe fn extend_lifetime<'a>(v: Dyn<'a>) -> Dyn<'static> {
    std::mem::transmute(v)
}

impl Future for Decontainer {
    type Item = ();
    type Error = ();

    fn poll<'a>(&'a mut self) -> Poll<Self::Item, Self::Error> {
        // Boilerplate to address pinning
        let mut_ref: std::pin::Pin<&'a mut DecontainerImpl> =
            Pin::as_mut(&mut self.pinned);
        let self_: &'a mut DecontainerImpl = unsafe {
            Pin::get_unchecked_mut(mut_ref)
        };

        assert!(self_.ec.new_ops.len() == 0);
        let mut ec: &'a mut ExecutionContext<'a> = unsafe {
            shorten_invariant_lifetime(&mut self_.ec)
        };
        loop {
            // Nothing left to do, so we are ready. Note that
            // Decontainer::new() adds a single future to machine_state, so
            // this is not empty initialarg_cbidly.
            if self_.machine_state.len() == 0 {
                return Result::Ok(Async::Ready(()));
            }
            let mut any_completed = false;
            for (completed, clos, future) in self_.machine_state.iter_mut() {
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
                        let f = &self_.func;
                        f(&self_.arena, &mut ec, Dyn::Int(0), unsafe { shorten_invariant_lifetime2(*clos) }).unwrap();
                    }
                }
            }
            // Nothing completed, thus never executed T::callback.
            if any_completed == false {
                return Result::Ok(Async::NotReady);
            }
            self_
                .machine_state
                .retain(|(completed, _, _)| *completed == false);
            // Create a future for each new operation.
            for (op, indicator, clos) in ec.new_ops.iter() {
                match op {
                    AsyncOp::Listen => {
                        // TODO(arjun): Bogus implementation
                        self_
                            .machine_state
                            .push((false, unsafe { extend_lifetime(*clos) }, Box::new(future::ok(*indicator))))
                    },
                    AsyncOp::Immediate => {
                        self_
                            .machine_state
                            .push((false, unsafe { extend_lifetime(*clos) }, Box::new(future::ok(*indicator))));
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

//! An implementation of a multi-producer, multi-consumer queue.
//!  
//! For some reason, this is not available as part of the futures library. It is
//! available as part of CrossBeam, but CrossBeam is not compatible with
//! futures.
//!
//! ``Queue::new()`` returns a sender and receiver that can both be cloned and
//! sent between threads.
//!
//! The implementation is not efficient, since it uses a single coarse-grained
//! lock. However, it should be good enough for our purposes.

use crate::error::Error;
use futures::sync::oneshot;
use futures::{future, Future};
use futures_locks::Mutex;
use std::collections::VecDeque;

// TODO(arjun): It would probably be straightforward to introduce a mutex
// around each field to improve performance. Alternatively, use a lock-free
// queue for both queue and pending_tasks. We only need oneshot::Sender for
// tasks that wait.
pub struct Queue<T> {
    queue: VecDeque<T>,
    pending_tasks: VecDeque<oneshot::Sender<T>>,
}

pub struct Sender<T> {
    queue: Mutex<Queue<T>>,
}

// TODO(arjun): This is apparently not the code that gets derived using
// #[derive(Clone)]. The derived code required <T : Clone>.
impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Sender { queue: self.queue.clone() }
    }
}

#[derive(Clone)]
pub struct Receiver<T> {
    queue: Mutex<Queue<T>>,
}

impl<T> Queue<T> {
    pub fn new() -> (Sender<T>, Receiver<T>) {
        let q = Queue {
            // NOTE(arjun): 64 seems like an okay default. Are we really going
            // to run more than 64 containers on a single machine?
            queue: VecDeque::with_capacity(64),
            // NOTE(arjun): This is the number of stalled requests. If we
            // hammer the machine, the buffer will grow.
            pending_tasks: VecDeque::with_capacity(128),
        };
        let send_mutex = Mutex::new(q);
        let recv_mutex = send_mutex.clone();
        return (Sender { queue: send_mutex }, Receiver { queue: recv_mutex });
    }
}

impl<T> Receiver<T> {
    pub fn recv_immediate(&self) -> Option<T> {
        match self.queue.try_lock() {
            Err(_) => None,
            Ok(mut q) => q.queue.pop_front(),
        }
    }

    pub fn recv(&self) -> impl Future<Item = T, Error = Error> {
        self.queue
            .lock()
            .from_err()
            .and_then(|mut q| match q.queue.pop_front() {
                Some(x) => future::Either::A(future::ok(x)),
                None => {
                    let (send, recv) = oneshot::channel();
                    q.pending_tasks.push_back(send);
                    future::Either::B(recv.from_err())
                }
            })
    }
}

impl<T> Sender<T> {
    pub fn send(&self, x: T) -> impl Future<Item = (), Error = ()> {
        return self.queue.lock().map(|mut q| {
            match q.pending_tasks.pop_front() {
                Some(task) => match task.send(x) {
                    Ok(_) => (),
                    // TODO(arjun): Think through why this would occur.
                    Err(_err) => panic!("send failed"),
                },
                None => {
                    q.queue.push_back(x);
                }
            }
        });
    }
}

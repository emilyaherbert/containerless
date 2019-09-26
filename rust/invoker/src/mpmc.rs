/// An implementation of a multi-producer, multi-consumer queue. For some
/// reason, this is not available as part of the futures library. It is
/// available as part of CrossBeam, but CrossBeam is not compatible with
/// futures.
///
/// ``Queue::new()`` returns a sender and receiver that can both be cloned and
/// sent between threads.
///
/// The implementation is not efficient, since it uses a single coarse-grained
/// lock. However, it should be good enough for our purposes.
use crate::error::Error;
use futures::sync::oneshot;
use futures::{Async, Future, Poll};
use futures_locks::{Mutex};
use std::collections::VecDeque;

// NOTE(arjun): It would probably be straightforward to introduce a mutex
// around each field to improve performance. Alternatively, use a lock-free
// queue for both queue and pending_tasks. We only need oneshot::Sender for
// tasks that wait.
pub struct Queue<T> {
    queue: VecDeque<T>,
    pending_tasks: VecDeque<oneshot::Sender<T>>,
}

#[derive(Clone)]
pub struct Sender<T> {
    queue: Mutex<Queue<T>>,
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
            // TODO(arjun): What determines this number?
            pending_tasks: VecDeque::with_capacity(128),
        };
        let send_mutex = Mutex::new(q);
        let recv_mutex = send_mutex.clone();
        return (Sender { queue: send_mutex }, Receiver { queue: recv_mutex });
    }
}

enum ReceiverFutData<T> {
    Immediate(Option<T>),
    Waiting(futures::future::FromErr<oneshot::Receiver<T>, Error>),
}

struct ReceiverFut<T> {
    data: ReceiverFutData<T>,
}

impl<T> ReceiverFut<T> {
    fn immediate(x: T) -> ReceiverFut<T> {
        return ReceiverFut {
            data: ReceiverFutData::Immediate(Some(x)),
        };
    }

    fn waiting(f: futures::future::FromErr<oneshot::Receiver<T>, Error>) -> ReceiverFut<T> {
        return ReceiverFut {
            data: ReceiverFutData::Waiting(f),
        };
    }
}
impl<T> Future for ReceiverFut<T> {
    type Item = T;
    type Error = Error;
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        match &mut self.data {
            ReceiverFutData::Immediate(x) => {
                let y = std::mem::replace(x, None);
                Ok(Async::Ready(y.expect("double poll")))
            }
            ReceiverFutData::Waiting(fut) => fut.poll(),
        }
    }
}

impl<T> Receiver<T> {
    pub fn recv(&self) -> impl Future<Item = T, Error = Error> {
        return self
            .queue
            .lock()
            .from_err()
            .and_then(|mut q| match q.queue.pop_front() {
                Some(x) => return ReceiverFut::immediate(x),
                None => {
                    let (send, recv) = oneshot::channel();
                    q.pending_tasks.push_back(send);
                    return ReceiverFut::waiting(recv.from_err());
                }
            });
    }
}

impl<T> Sender<T> {
    pub fn send(&self, x: T) -> impl Future<Item = (), Error = ()> {
        return self.queue.lock().map(|mut q| {
            match q.pending_tasks.pop_front() {
                Some(task) => match task.send(x) {
                    Ok(_) => (),
                    Err(_err) => panic!("send failed"),
                },
                None => {
                    q.queue.push_back(x);
                }
            }
        });
    }
}

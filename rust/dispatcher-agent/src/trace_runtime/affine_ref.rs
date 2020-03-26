//! An `AffineBoxFactory` allows values are do not implement `Send` to be shared
//! across multiple threads, by dynamically ensuring that they are only accessed
//! from one thread at a time.
//!
//! Recall that the built-in type `Cell<T>` does not implement `Sync`, thus a
//! value of type `&Cell<T>` does not implement `Send`, and cannot be shared
//! across threads. This restriction prevents data races on the contents of
//! the cell. So, the only safe way to share a `&Cell<T>` across threads is
//! to wrap it in a mutex (`Mutex<&'a Cell<T>>`).
//!
//! This library guards several cells using the same mutex. The type
//! `AffineBox<T>` is (unsafely) marked `Send`, even if its contents are not
//! sendable. However, the `read` and `read_mut` methods ensure that all reads
//! occur from the same thread, and otherwise panic. The reading thread must
//! call `AffineBoxFactory::begin_reads` and `AffineBoxFactory::end_reads`
//! before and after calling the reading methods.
use std::sync::{Arc, Mutex};
use std::thread;

#[derive(Clone)]
pub struct AffineBoxFactory {
    owner: Arc<Mutex<Option<thread::ThreadId>>>,
}

pub struct AffineBox<T> {
    item: T,
    factory: AffineBoxFactory,
}

unsafe impl<T> Send for AffineBox<T> {}

impl AffineBoxFactory {
    pub fn new() -> AffineBoxFactory {
        let factory = AffineBoxFactory {
            owner: Arc::new(Mutex::new(None)),
        };
        return factory;
    }

    pub fn make_box<T>(&self, item: T) -> AffineBox<T> {
        return AffineBox {
            item,
            factory: self.clone(),
        };
    }

    fn get_owner(&self) -> thread::ThreadId {
        let result = self.owner.try_lock();
        if let Ok(guard) = result {
            return guard.expect("AffineBoxFactory is not owned by any thread");
        }
        panic!("concurrent calls to AffineBoxFactory::get_owner (by AffineBox::read or AffineBox::read_mut from another thread)");
    }

    pub fn begin_reads(&self) {
        let result = self.owner.try_lock();
        if let Ok(mut guard) = result {
            match *guard {
                None => *guard = Some(thread::current().id()),
                Some(tid) => panic!("AffineBoxFactory is already owned by thread {:?}", tid),
            }
        } else {
            panic!("concurrent calls to AffineBoxFactory::begin_reads");
        }
    }

    pub fn end_reads(&self) {
        let result = self.owner.try_lock();
        if let Ok(mut guard) = result {
            match *guard {
                None => panic!("AffineBoxFactory is not owned by any thread"),
                Some(tid) => {
                    assert!(tid == thread::current().id(), "AffineBoxFactory is owned by thread {:?}, which must be the thread that releases it", tid);
                    *guard = None;
                }
            }
        } else {
            panic!("concurrent calls to AffineBoxFactory::end_reads");
        }
    }
}

impl<T> AffineBox<T> {
    pub fn read<'a>(&'a self) -> &'a T {
        assert!(
            self.factory.get_owner() == thread::current().id(),
            "AffineBox::read called from the wrong thread"
        );
        return &self.item;
    }

    pub fn read_mut<'a>(&'a mut self) -> &'a mut T {
        assert!(
            self.factory.get_owner() == thread::current().id(),
            "AffineBox::read_mut called from the wrong thread"
        );
        return &mut self.item;
    }
}

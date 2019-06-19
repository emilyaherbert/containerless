use hyper::Client;
use futures::{Future, Poll, Async};
// use bumpalo::Bump;
// use std::sync::{Arc, Mutex};

struct MiniTask {
    callback : Box<fn(&mut ExecutionContext) -> ()>,
    future: Box<Future<Item = (), Error = ()> + Send>,
    completed: bool
}

pub struct Runtime {
    tasks: Vec<MiniTask>,
    arena: u32 // TODO(arjun): Does not work -- Arc<Mutex<Bump>>>
}

pub struct ExecutionContext {
    new_tasks: Vec<MiniTask>
}

impl Runtime {

    /**
     * Creates a new runtime for a program that starts at main. The program
     * receives an 'ExecutionContext' as an argument, which it can use to
     * perform asynchronous I/O and execute callbacks when I/O completes.
     * However, the runtime ensures that callbacks never execute in parallel.
     */
    pub fn new(main: fn(&mut ExecutionContext) -> ()) -> Runtime {
        let main_task = MiniTask {
            callback: Box::new(main),
            future: Box::new(futures::done(Ok(()))),
            completed: false
        };
        Runtime {
            tasks: vec! [main_task],
            arena: 0 // Arc::new(Mutex::new(bumpalo::Bump::new())),
        }
    }
}

impl Future for Runtime {
    type Item = ();
    type Error = ();
    fn poll(
        &mut self
    ) -> Poll<Self::Item, Self::Error> {
        let mut ec = ExecutionContext {
            new_tasks: Vec::new()
        };

        loop {
            if self.tasks.is_empty() {
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
                        f(&mut ec);
                        any_ready = true;
                    }
                }
            }

            self.tasks.append(&mut ec.new_tasks);
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
    pub fn request(
        &mut self,
        uri: &str,
        callback: fn(&mut ExecutionContext) -> ()) {
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
        let mini_task = MiniTask {
            callback : Box::new(callback),
            future : Box::new(resp),
            completed: false
        };
        self.new_tasks.push(mini_task);
    }

}

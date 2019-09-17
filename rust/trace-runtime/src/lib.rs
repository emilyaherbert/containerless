use bumpalo::Bump;

pub mod error;
pub mod execution_context;
pub mod type_dynamic;
pub mod types;

pub use error::*;
pub use execution_context::*;
pub use type_dynamic::*;

pub type ContainerlessFunc<'a> = fn(
    ec: &mut ExecutionContext,
    arg_cbid: Dyn<'a>,
    arg_cbargs: Dyn<'a>,
) -> DynResult<'a>;

pub struct Decontainerized<'a> {
    arena: Bump,
    ec: ExecutionContext,
    f: for <'b> ContainerlessFunc<'b>
}

pub fn unknown<T>() -> Result<T, Error> {
    Err(Error::Unknown)
}

impl<'a> Decontainerized<'a> {

    pub fn poll(&mut self) -> () {
        let mut loopback_id = 0;
        let cbargs = Dyn::vec(&self.arena);
        // Dummy value for the "global closure". It does not matter what this is.
        cbargs.push(Dyn::int(0));
        // The actual argument
        cbargs.push(Dyn::int(1));
        // TODO(arjun): This is the response_callback. I have a feeling this is
        // junk
        cbargs.push(Dyn::int(2));
        loop {
            let fun = self.f;
            fun(&mut self.ec,
                Dyn::int(loopback_id),
                cbargs).expect("function failed");
            if let Some((event_name, new_loopback_id)) = self.ec.events.pop() {
                loopback_id = new_loopback_id;
                println!("Processing event {} with id {}", event_name, new_loopback_id);
            }
            else {
                return;
            }
        }
    }

    pub fn new(f: ContainerlessFunc<'a>) -> Decontainerized<'a> {
        return Decontainerized {
            arena: Bump::new(),
            ec: ExecutionContext::new(),
            f: f
        };
    }
}


#[cfg(test)]
mod tests {

    use super::*;
    use bumpalo::Bump;
    use futures::{
        future::{lazy, ok},
        sync::oneshot,
        Future,
    };
    use tokio::runtime::Runtime;
    use types::*;

    pub struct ExampleState<'a> {
        x: &'a i32,
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

    pub enum ExampleTask {}

    impl Decontainerized for ExampleTask {
        type StateFamily = ExampleStateFamily;
        fn new<'a>(arena: &'a Bump) -> <Self::StateFamily as FamilyLt<'a>>::Out
        where
            Self::StateFamily: FamilyLt<'a>,
        {
            let y = arena.alloc(100);
            return ExampleStateFamily::from(ExampleState { x: y });
        }

        fn callback<'a>(
            arena: &'a Bump,
            state: &'a mut <Self::StateFamily as FamilyLt<'a>>::Out,
            ec: &mut ExecutionContext,
        ) -> ()
        where
            Self::StateFamily: FamilyLt<'a>,
        {
            let state = ExampleStateFamily::to(state);
            let y = arena.alloc(*state.x + 1);
            state.x = y;
            if *state.x == 150 {
                return;
            }
            ec.loopback(AsyncOp::Immediate, 1);
        }
    }

    #[test]
    fn test_runtime() {
        let mut rt = Runtime::new().unwrap();
        let (tx, rx) = oneshot::channel();
        rt.spawn(lazy(|| {
            let task = Decontainer::new() as Decontainer<ExampleTask>;
            return task.then(|_r| tx.send(()));
        }));
        rx.wait().unwrap();
        rt.shutdown_now().wait().expect("could not shutdown Tokio");
        assert_eq!(2 + 2, 4);
    }
}

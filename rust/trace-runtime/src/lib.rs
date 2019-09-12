pub mod error;
pub mod execution_context;
pub mod type_dynamic;
pub mod types;

pub use error::*;
pub use execution_context::*;
pub use type_dynamic::*;

pub type ContainerlessFunc<'a> = fn(
    ec: &mut ExecutionContext,
    arena: &'a bumpalo::Bump,
    arg_cbid: DynResult<'a>,
    arg_cbargs: DynResult<'a>,
) -> DynResult<'a>;

pub fn unknown<T>() -> Result<T, Error> {
    Err(Error::Unknown)
}

use bumpalo::Bump;

pub fn init<'a>(arena: &'a Bump, f: ContainerlessFunc<'a>) -> DynResult<'a> {

    let mut ec = ExecutionContext::new();
    let mut loopback_id = 0;
    loop {
        f(&mut ec,
            &arena,
            Dyn::int(loopback_id),
            Dyn::int(0)).expect("function failed");
        if let Some((event_name, new_loopback_id)) = ec.events.pop() {
            loopback_id = new_loopback_id;
            println!("Processing event {} with id {}", event_name, new_loopback_id);
        }
        else {
            return Dyn::int(0);
        }
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

pub mod error;
pub mod execution_context;
pub mod type_dynamic;
pub mod types;
// pub mod trial;

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

// use bumpalo::Bump;

// pub fn init<'a>(arena: &'a Bump, f: ContainerlessFunc<'a>) -> DynResult<'a> {

//     let mut ec = ExecutionContext::new();
//     let mut loopback_id = 0;
//     loop {
//         f(&mut ec,
//             &arena,
//             Dyn::int(loopback_id),
//             Dyn::int(0)).expect("function failed");
//         if let Some((event_name, new_loopback_id)) = ec.events.pop() {
//             loopback_id = new_loopback_id;
//             println!("Processing event {} with id {}", event_name, new_loopback_id);
//         }
//         else {
//             return Dyn::int(0);
//         }
//     }
// }

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

    
    fn callback<'a>(
      arena: &'a Bump,
      ec: &mut crate::ExecutionContext<'a>,
      clos: Dyn<'a>) -> () {
      println!("Seems to work");
    }

    #[test]
    fn test_runtime() {
        let mut rt = Runtime::new().unwrap();
        let (tx, rx) = oneshot::channel();
        rt.spawn(lazy(|| {
            let task = Decontainer::new(Box::new(callback));
            return task.then(|_r| tx.send(()));
        }));
        rx.wait().unwrap();
        rt.shutdown_now().wait().expect("could not shutdown Tokio");
        assert_eq!(2 + 2, 4);
    }
}

use serde::Serialize;
use std::sync::Arc;
use tokio::sync::Mutex;

#[derive(Copy, Clone, PartialEq, Debug, Serialize)]
pub enum State {
    Preinitialized,
    Compiling,
    CompileError,
    Running,
    RuntimeError,
}

#[derive(Clone)]
pub struct StateHandle {
    state: Arc<Mutex<State>>,
}

impl StateHandle {
    pub fn new() -> StateHandle {
        return StateHandle {
            state: Arc::new(Mutex::new(State::Preinitialized)),
        };
    }

    pub async fn get_state(&self) -> State {
        let s = self.state.lock().await;
        return *s;
    }

    pub async fn set_compiling(&self) -> bool {
        let mut s = self.state.lock().await;
        if let State::Preinitialized = *s {
            *s = State::Compiling;
            return true;
        }
        return false;
    }

    pub async fn set_running(&self) -> () {
        let mut s = self.state.lock().await;
        if let State::Compiling = *s {
            *s = State::Running;
            return;
        }
        panic!("set_running in {:?}", *s);
    }

    pub async fn set_compile_error(&self) -> () {
        let mut s = self.state.lock().await;
        if let State::Compiling = *s {
            *s = State::CompileError;
            return;
        }
        panic!("set_compile_error in {:?}", *s);
    }

    pub async fn set_runtime_error(&self) -> () {
        let mut s = self.state.lock().await;
        if let State::Running = *s {
            *s = State::RuntimeError;
            return;
        }
        panic!("set_runtime_error in {:?}", *s);
    }
}

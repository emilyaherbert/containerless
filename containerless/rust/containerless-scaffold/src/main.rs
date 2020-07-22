use invoker;
mod containerless;

pub fn main() {
    invoker::main(Some(containerless::containerless));
}
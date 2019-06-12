#[derive(Debug)]
pub enum DatastoreError {
    JSONError,
    EntityParseError,
    KeyParseError
}
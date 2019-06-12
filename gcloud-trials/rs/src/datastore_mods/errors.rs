use std::fmt;

#[derive(Debug)]
pub enum DatastoreError {
    JSONError,
    EntityParseError,
    KeyParseError
}

impl fmt::Display for DatastoreError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut str = "";
        match &self {
            DatastoreError::JSONError => "JSONError",
            DatastoreError::EntityParseError => "EntityParseError",
            DatastoreError::KeyParseError => "KeyParseError"
        };
        Ok(())
    }
}
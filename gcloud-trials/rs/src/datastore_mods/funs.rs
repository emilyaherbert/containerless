#[warn(unused_must_use)]

use datastore_mods::{
    ds::DS,
    user::User
};

use serde_json::{
    Value
};

#[derive(Debug)]
enum DatastoreError {
    JSONError
}

fn check_fields(request_data: &Value) -> Result<String, DatastoreError> {
    match request_data["username"] {
        Value::Null => Err(DatastoreError::JSONError),
        _ => {
            match request_data["password"] {
                Value::Null => Err(DatastoreError::JSONError),
                _ => Ok("Ok!".to_string())
            }
        }
    }
}

pub fn register(ds: DS, request_data: &Value) {
    match check_fields(request_data) {
        Err(err) => println!("{:?}", err),
        Ok(_) => {
            let user =
                User::new(
                    &ds.project_name,
                    // NOTE(emily): You have to do this weird conversion method to avoid extra newlines...
                    request_data["username"].as_str().unwrap().to_string(),
                    request_data["password"].as_str().unwrap().to_string()
                );

            ds.insert(&user)
            .and_then(|ok| {
                println!("User {:?} registered.", request_data["username"].as_str().unwrap().to_string());
                Ok(ok)
            })
            .or_else(|err| {
                println!("{:?}", err);
                Err(err)
            });
        }
    }
}
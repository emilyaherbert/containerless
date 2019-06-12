#[warn(unused_must_use)]

use datastore_mods::{
    ds::DS,
    user::User,
    user::UserKey,
    errors::DatastoreError
};

use serde_json::{
    Value
};

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

pub fn register(ds: &DS, request_data: &Value) {
    match check_fields(request_data) {
        Err(err) => println!("{:?}", err),
        Ok(_) => {
            let user =
                User::new(
                    &ds.project_name,
                    // NOTE(emily): You have to do this weird conversion method to avoid extra newlines...
                    &request_data["username"].as_str().unwrap().to_string(),
                    &request_data["password"].as_str().unwrap().to_string()
                );

            ds.upsert(&user)
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

fn authorize<F>(ds: &DS, request_data: &Value, next: F) where F: Fn(&Value) {
    match check_fields(request_data) {
        Err(err) => println!("{:?}", err),
        Ok(_) => {
            let user_key =
                UserKey::new(
                    &ds.project_name,
                    &request_data["username"].as_str().unwrap().to_string()
                );

            ds.get(&user_key)
            .and_then(|ok| {
                match &ok {
                    Some(entity) => {
                        match User::from_entity(entity) {
                            Err(err) => println!("{:?}", err),
                            Ok(user) => {
                                if(request_data["password"].as_str().unwrap().to_string() == user.password) {
                                    next(request_data);
                                } else {
                                    println!("Incorrect password!");
                                }
                            }
                        }
                    }
                    None => println!("User not found!")
                };
                Ok(ok)
            })
            .or_else(|err| {
                println!("{:?}", err);
                Err(err)
            });
        }
    }

}

pub fn login(ds: &DS, request_data: &Value) {
    authorize(ds, request_data, |req| {
        println!("Login successful!");
    })
}
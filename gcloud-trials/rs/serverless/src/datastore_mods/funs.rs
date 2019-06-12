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

pub fn register(ds: &DS, request_data: &Value) -> String {
    let mut ret = "".to_string();
    match check_fields(request_data) {
        Err(err) => ret.push_str(&err.to_string()),
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
                ret.push_str("User ");
                ret.push_str(&request_data["username"].as_str().unwrap().to_string());
                ret.push_str(" registered.");
                Ok(ok)
            })
            .or_else(|err| {
                ret.push_str(&err.to_string());
                Err(err)
            });
        }
    };
    return ret;
}

fn authorize(ds: &DS, request_data: &Value, next: impl Fn(&Value)->String) -> String {
    let mut ret = "".to_string();
    match check_fields(request_data) {
        Err(err) => ret.push_str(&err.to_string()),
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
                            Err(err) => ret.push_str(&err.to_string()),
                            Ok(user) => {
                                if(request_data["password"].as_str().unwrap().to_string() == user.password) {
                                    next(request_data);
                                } else {
                                    ret.push_str("Incorrect password!");
                                }
                            }
                        }
                    },
                    None => ret.push_str("User not found!")
                };
                next(request_data);
                Ok(ok)
            })
            .or_else(|err| {
                ret.push_str(&err.to_string());
                Err(err)
            });
        }
    };
    return ret;
}

pub fn login(ds: &DS, request_data: &Value) -> String {
    return authorize(ds, request_data, |req| {
        return ("Login successful!".to_string());
    });
}
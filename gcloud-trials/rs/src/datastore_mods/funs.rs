use datastore_mods::{
    ds
};

use google_datastore1::{
    Key,
    Entity
};

use serde_json::{
    Value
};

use std::collections::HashMap;

#[derive(Debug)]
enum DatastoreError {
    JSONError
}

fn get_key_from_request_data(datastore: &ds::DS, request_data: &Value) -> Key  {
    datastore.key("User".to_string(), request_data["username"].as_str().unwrap().to_string())
}

fn value_from_string(string: String) -> google_datastore1::Value {
    google_datastore1::Value {
        entity_value: None,
        timestamp_value: None,
        string_value: Some(string),
        double_value: None,
        meaning: None,
        exclude_from_indexes: None,
        blob_value: None,
        key_value: None,
        boolean_value: None,
        array_value: None,
        integer_value: None,
        geo_point_value: None,
        null_value: None
    }
}

fn get_properties_from_request_data(request_data: &Value) -> HashMap<String, google_datastore1::Value> {
    let mut props: HashMap<String, google_datastore1::Value> = HashMap::new();

    props.insert(
        "username".to_string(),
        value_from_string(request_data["username"].as_str().unwrap().to_string())
    );

    props.insert(
        "password".to_string(),
        value_from_string(request_data["password"].as_str().unwrap().to_string())
    );

    props
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

pub fn register(datastore: &ds::DS, request_data: &Value) {
    match check_fields(request_data) {
        Err(err) => println!("{:?}", err),
        Ok(_) => {
            let key = get_key_from_request_data(datastore, request_data);
            let props = get_properties_from_request_data(request_data);

            let entity =
                Entity {
                    key: Some(key),
                    properties: Some(props)
                };

            let _ = datastore
            .save(entity)
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
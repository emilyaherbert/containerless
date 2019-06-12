
// https://github.com/n-k/dstest/blob/master/src/models.rs

use datastore_mods::{
    errors::DatastoreError
};

use google_datastore1::{
    Entity,
    Key,
    PathElement,
    PartitionId
};

use std::{
    collections::HashMap
};

#[derive(Debug)]
pub struct UserKey {
    pub key: Key
}

impl UserKey {
    pub fn new(project_name: &String, username: &String) -> UserKey {
        let key =
                Key {
                path: Some(
                    vec![PathElement {
                        kind: Some("User".to_string()),
                        id: None,
                        name: Some(username.clone())
                    }]
                ),
                partition_id: Some(
                    PartitionId {
                        project_id: Some(project_name.clone()),
                        namespace_id: None
                    }
                )
            };
        return UserKey { key: key };
    }

    pub fn from_key(key: &Key) -> Result<UserKey, DatastoreError> {
        match key.partition_id {
            None => Err(DatastoreError::KeyParseError),
            Some(ref partition_id) => {
                match partition_id.project_id {
                    None => Err(DatastoreError::KeyParseError),
                    Some(ref project_name) => {
                        match key.path {
                            None => Err(DatastoreError::KeyParseError),
                            Some(ref paths) => {
                                match paths.first() {
                                    None => Err(DatastoreError::KeyParseError),
                                    Some(path) => {
                                        match path.clone().name {
                                            None => Err(DatastoreError::KeyParseError),
                                            Some(username) => {
                                                return Ok(UserKey::new(&project_name, &username));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn get_key(&self) -> Key {
        return self.key.clone();
    }
}

#[derive(Debug)]
pub struct User {
    pub key: UserKey,
    pub username: String,
    pub password: String
}

impl User {

    pub fn new(project_name: &String, username: &String, password: &String) -> User {
        let key = UserKey::new(project_name, username);
        return User { key: key, username: username.to_string(), password: password.to_string() };
    }

    pub fn from_entity(entity: &Entity) -> Result<User, DatastoreError> {
        match entity.key {
            None => Err(DatastoreError::EntityParseError),
            Some(ref key) => {
                match UserKey::from_key(&key) {
                    Err(err) => Err(err),
                    Ok(user_key) => {
                        match entity.properties {
                            None => Err(DatastoreError::EntityParseError),
                            Some(ref properties) => {
                                if(properties.contains_key("username") && properties.contains_key("password")) {
                                    match (properties.get("username"), properties.get("password")) {
                                        (Some(username), Some(password)) => {
                                            match (&username.string_value, &password.string_value) {
                                                (Some(u), Some(p)) => {
                                                    return Ok( User { key: user_key, username: u.to_string(), password: p.to_string() } );
                                                },
                                                _ => Err(DatastoreError::EntityParseError)
                                            }
                                        },
                                        _ => Err(DatastoreError::EntityParseError)
                                    }
                                } else {
                                    return Err(DatastoreError::EntityParseError);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn get_key(&self) -> Key {
        return self.key.key.clone();
    }

    pub fn to_entity(&self) -> Entity {
        let mut props = HashMap::new();
        props.insert(
            "username".to_string(),
            google_datastore1::Value {
                entity_value: None,
                timestamp_value: None,
                string_value: Some(self.username.to_string()),
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
            });
        props.insert(
            "password".to_string(),
            google_datastore1::Value {
                entity_value: None,
                timestamp_value: None,
                string_value: Some(self.password.to_string()),
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
            });

        Entity {
            key: Some(self.get_key()),
            properties: Some(props)
        }
    }
}
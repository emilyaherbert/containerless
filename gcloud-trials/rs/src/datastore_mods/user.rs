
// https://github.com/n-k/dstest/blob/master/src/models.rs

use google_datastore1::{
    Entity,
    Key,
    PathElement,
    PartitionId
};

use std::{
    collections::HashMap
};

pub struct User {
    pub project_name: String,
    pub username: String,
    pub password: String
}

impl User {

    pub fn get_key(&self) -> Key {
        Key {
            path: Some(
                vec![PathElement {
                    kind: Some("User".to_string()),
                    id: None,
                    name: Some(self.username.to_string())
                }]
            ),
            partition_id: Some(
                PartitionId {
                    project_id: Some(self.project_name.to_string()),
                    namespace_id: None
                }
            )
        }
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
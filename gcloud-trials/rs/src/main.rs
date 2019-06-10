#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_mut)]

extern crate yup_oauth2 as oauth2;
extern crate hyper;
extern crate hyper_rustls;
extern crate google_datastore1;
extern crate serde_json;

mod datastore_mods;

use std::env;

use serde_json::{
    Value,
    json
};

use google_datastore1::{
    Datastore,
    Error,
    Key,
    PathElement,
    Entity,
    LookupRequest,
    LookupResponse,
    CommitRequest,
    Mutation,
    Result,
    EntityResult,
    CommitResponse,
    PartitionId,
    BeginTransactionRequest,
    TransactionOptions,
    ReadWrite
};

use datastore_mods::{
    ds,
    funs
};

use std::collections::HashMap;

// https://docs.rs/google-datastoprojectIdre1/1.0.8+20181002/google_datastore1/
// https://github.com/n-k/dstest

fn main() {

    let google_app_creds = env::var("GOOGLE_APPLICATION_CREDENTIALS")
        .expect("No GOOGLE_APPLICATION_CREDENTIALS environment variable found");
    let project_name = env::var("PROJECT_NAME")
        .expect("No PROJECT_NAME environment variable found");
    let datastore = ds::DS::new(google_app_creds,project_name);

    let mut props = HashMap::new();
    props.insert(
        "username".to_string(),
        google_datastore1::Value {
            entity_value: None,
            timestamp_value: None,
            string_value: Some("pop".to_string()),
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
    );
    props.insert(
        "password".to_string(),
        google_datastore1::Value {
            entity_value: None,
            timestamp_value: None,
            string_value: Some("tarts".to_string()),
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
    );

    let request =
        BeginTransactionRequest {
            transaction_options: Some(
                TransactionOptions {
                    read_write: Some(
                        ReadWrite {
                            previous_transaction: None
                        }
                    ),
                    read_only: None
                }
            )
        };

    match datastore.ds.projects().begin_transaction(request, "umass-plasma").doit() {
        Err(err) => println!("{:?}", err),
        Ok(ok) => {
            let (foo, bar) = ok;
            println!("{:?}", bar.transaction);
            let request =
                CommitRequest {
                    mode: Some("TRANSACTIONAL".to_string()),
                    transaction: bar.transaction,
                    mutations: Some(vec![
                        Mutation {
                            upsert: None,
                            delete: None,
                            update: None,
                            insert: Some(
                                Entity {
                                    key: Some(
                                        Key {
                                            path: Some(
                                                vec![PathElement {
                                                    kind: Some("User".to_string()),
                                                    id: None,
                                                    name: Some("pop2".to_string())
                                                }]
                                            ),
                                            partition_id: Some(
                                                PartitionId {
                                                    project_id: Some("umass-plasma".to_string()),
                                                    namespace_id: None
                                                }
                                            )
                                        }
                                    ),
                                    properties: Some(props)
                                }
                            ),
                            base_version: None
                        }
                    ])
                };
            let res = datastore.ds.projects().commit(request, "umass-plasma").doit();
            println!("\n{:?}\n", res);
        }
    }

    /*
    let request =
        BeginTransactionRequest {
            transaction_options: Some(
                TransactionOptions {
                    read_write: Some(
                        ReadWrite {
                            previous_transaction: None
                        }
                    ),
                    read_only: None
                }
            )
        };

    match datastore.ds.projects().begin_transaction(request, "umass-plasma").doit() {
        Err(err) => println!("{:?}", err),
        Ok(ok) => {
            let (foo, bar) = ok;
            match datastore.lookup_one() {
                Err(err) => println!("{:?}", err),
                Ok(ok) => {
                    match ok {
                        None => println!("None!"),
                        Some(entity) => {
                            let request =
                                CommitRequest {
                                    mode: Some("TRANSACTIONAL".to_string()),
                                    transaction: bar.transaction,
                                    mutations: Some(vec![
                                        Mutation {
                                            upsert: None,
                                            delete: None,
                                            update: None,
                                            insert: Some(entity),
                                            base_version: None
                                        }
                                    ]),
                                };
                            let res = datastore.ds.projects().commit(request, "umass-plasma").doit();
                            println!("{:?}", res)
                        }
                    }
                }
            }
        }
    }
    */

    /*
    let request = json!({
        "username":"pop",
        "password":"tart"
    });

    funs::register(&datastore, &request);
    */

}
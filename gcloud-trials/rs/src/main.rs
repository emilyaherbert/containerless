#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_mut)]

extern crate yup_oauth2 as oauth2;
extern crate hyper;
extern crate hyper_rustls;
extern crate google_datastore1;
extern crate serde_json;

use std::env;

use hyper::{
    net::HttpsConnector,
    Client,
    client::response::Response
};

use hyper_rustls::{
    TlsClient
};

use oauth2::{
    ServiceAccountAccess,
    Authenticator,
    DefaultAuthenticatorDelegate,
    ApplicationSecret,
    MemoryStorage,
    ServiceAccountKey,
    GetToken,
    FlowType
};

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
    ReadWrite,
};

use std::collections::HashMap;

use std::default::Default;

// https://docs.rs/google-datastoprojectIdre1/1.0.8+20181002/google_datastore1/

fn main() {

    let key_file = env::var("GOOGLE_APPLICATION_CREDENTIALS")
        .expect("No GOOGLE_APPLICATION_CREDENTIALS environment variable found");
    let project_name = env::var("PROJECT_NAME")
        .expect("No PROJECT_NAME environment variable found");

    
    let client_secret: ServiceAccountKey = oauth2::service_account_key_from_file(&key_file)
        .unwrap();
    let client = Client::with_connector(HttpsConnector::new(TlsClient::new()));
    let access = ServiceAccountAccess::new(
        client_secret,
        client
    );
    let client = Client::with_connector(HttpsConnector::new(TlsClient::new()));
    let ds = Datastore::new(client, access);
    
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
        CommitRequest {
            mode: Some("NON_TRANSACTIONAL".to_string()),
            transaction: None,
            mutations: Some(vec![
                Mutation {
                    insert: None,
                    delete: None,
                    update: None,
                    upsert: Some(
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
    let res = ds.projects().commit(request, "umass-plasma").doit();
    println!("\n{:?}\n", res);

}
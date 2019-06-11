#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_must_use)]

extern crate yup_oauth2 as oauth2;
extern crate hyper;
extern crate hyper_rustls;
extern crate google_datastore1;
extern crate serde_json;

mod datastore_mods;

use datastore_mods::{
    user::User,
    user::UserKey,
    ds::DS,
    funs
};

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

use std::{
    env,
    collections::HashMap,
    default::Default
};

// https://docs.rs/google-datastoprojectIdre1/1.0.8+20181002/google_datastore1/

fn main() {

    let key_file = env::var("GOOGLE_APPLICATION_CREDENTIALS")
        .expect("No GOOGLE_APPLICATION_CREDENTIALS environment variable found");
    let project_name = env::var("PROJECT_NAME")
        .expect("No PROJECT_NAME environment variable found");
    let ds = DS::new(key_file, &project_name);
    
    let input = json!({
        "username": "Edward6",
        "password": "Snowden6"
    });

    funs::register(ds, &input);

}
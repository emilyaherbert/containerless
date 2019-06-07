#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(unused_mut)]

extern crate yup_oauth2 as oauth2;
extern crate hyper;
extern crate hyper_rustls;
extern crate google_datastore1;

use std::env;

pub mod ds;

// https://docs.rs/google-datastoprojectIdre1/1.0.8+20181002/google_datastore1/
// https://github.com/n-k/dstest

fn main() {

    let google_app_creds = env::var("GOOGLE_APPLICATION_CREDENTIALS")
        .expect("No GOOGLE_APPLICATION_CREDENTIALS environment variable found");
    let project_name = env::var("PROJECT_NAME")
        .expect("No PROJECT_NAME environment variable found");
    let ds = ds::DS::new(google_app_creds,project_name);

    ds.lookup_one();

}
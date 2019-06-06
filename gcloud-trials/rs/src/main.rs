#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

extern crate hyper;
extern crate hyper_rustls;
extern crate yup_oauth2 as oauth2;
extern crate google_datastore1 as datastore1;
use datastore1::GoogleDatastoreAdminV1ExportEntitiesRequest;
use datastore1::{Result, Error};
use std::default::Default;
use oauth2::{Authenticator, DefaultAuthenticatorDelegate, ApplicationSecret, MemoryStorage};
use datastore1::Datastore;

fn main() {

    // Get an ApplicationSecret instance by some means. It contains the `client_id` and 
    // `client_secret`, among other things.
    let secret: ApplicationSecret = Default::default();
    // Instantiate the authenticator. It will choose a suitable authentication flow for you, 
    // unless you replace  `None` with the desired Flow.
    // Provide your own `AuthenticatorDelegate` to adjust the way it operates and get feedback about 
    // what's going on. You probably want to bring in your own `TokenStorage` to persist tokens and
    // retrieve them from storage.
    let auth = Authenticator::new(&secret, DefaultAuthenticatorDelegate,
                                hyper::Client::with_connector(hyper::net::HttpsConnector::new(hyper_rustls::TlsClient::new())),
                                <MemoryStorage as Default>::default(), None);
    let mut hub = Datastore::new(hyper::Client::with_connector(hyper::net::HttpsConnector::new(hyper_rustls::TlsClient::new())), auth);
    // As the method needs a request, you would usually fill it with the desired information
    // into the respective structure. Some of the parts shown here might not be applicable !
    // Values shown here are possibly random and not representative !
    let mut req = GoogleDatastoreAdminV1ExportEntitiesRequest::default();
    
    // You can configure optional parameters by calling the respective setters at will, and
    // execute the final call using `doit()`.
    // Values shown here are possibly random and not representative !
    let result = hub.projects().export(req, "projectId")
                .doit();
    
    match result {
        Err(e) => match e {
            // The Error enum provides details about what exactly happened.
            // You can also just use its `Debug`, `Display` or `Error` traits
            Error::HttpError(_)
            |Error::MissingAPIKey
            |Error::MissingToken(_)
            |Error::Cancelled
            |Error::UploadSizeLimitExceeded(_, _)
            |Error::Failure(_)
            |Error::BadRequest(_)
            |Error::FieldClash(_)
            |Error::JsonDecodeError(_, _) => println!("{}", e),
        },
        Ok(res) => println!("Success: {:?}", res),
    }

}
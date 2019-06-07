use hyper::{
    net::HttpsConnector,
    Client
};

use hyper_rustls::{
    TlsClient
};

use oauth2::{
    ServiceAccountAccess
};

use google_datastore1::{
    Datastore,
    Error,
    Key,
    PathElement,
    Entity,
    LookupRequest,
    CommitRequest,
    Mutation,
};

pub struct DS {
    ds: Datastore<Client, ServiceAccountAccess<Client>>,
    project: String
}

impl DS {

    pub fn new(key_file: String, project: String) -> DS {
        let client_secret = oauth2::service_account_key_from_file(&key_file).unwrap();
        let client = Client::with_connector(HttpsConnector::new(TlsClient::new()));
        let access = ServiceAccountAccess::new(client_secret, client);
        let client = Client::with_connector(HttpsConnector::new(TlsClient::new()));
        let hub = Datastore::new(client, access);
        DS {ds: hub, project: project}
    }

    pub fn lookup_one(&self) -> bool {
        let mut req = LookupRequest {
            keys: Some(vec![
                Key {
                    path: Some(vec![
                        PathElement {
                            kind: Some("User".to_string()),
                            name: Some("emily".to_string()),
                            id: None
                        }
                    ]),
                    partition_id: None
                }
            ]),
            read_options: None
        };

        let result = self.ds
            .projects()
            .lookup(req, &self.project)
            .doit();

        match result {
            Err(e) =>
                match e {
                    // The Error enum provides details about what exactly happened.
                    // You can also just use its `Debug`, `Display` or `Error` traits
                    Error::HttpError(_) => println!("HttpError"),
                    |Error::MissingAPIKey => println!("MissingAPIKey"),
                    |Error::MissingToken(_) => println!("MissingToken"),
                    |Error::Cancelled => println!("Cancelled"),
                    |Error::UploadSizeLimitExceeded(_, _) => println!("UploadSizeLimitExceeded"),
                    |Error::Failure(_) => println!("Failure"),
                    |Error::BadRequest(_) => println!("BadRequest"),
                    |Error::FieldClash(_) => println!("FieldClash"),
                    |Error::JsonDecodeError(_, _) => println!("{}", e),
                },
            Ok(res) => println!("Success: {:?}", res),
        };

        true
    }

}
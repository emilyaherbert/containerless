use hyper::{
    net::HttpsConnector,
    Client,
    client::response::Response
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
    LookupResponse,
    CommitRequest,
    Mutation,
    Result,
    EntityResult
};

pub struct DS {
    ds: Datastore<Client, ServiceAccountAccess<Client>>,
    project: String
}

impl DS {

    // https://github.com/n-k/dstest
    pub fn new(key_file: String, project: String) -> DS {
        let client_secret = oauth2::service_account_key_from_file(&key_file).unwrap();
        let client = Client::with_connector(HttpsConnector::new(TlsClient::new()));
        let access = ServiceAccountAccess::new(client_secret, client);
        let client = Client::with_connector(HttpsConnector::new(TlsClient::new()));
        let hub = Datastore::new(client, access);
        DS {ds: hub, project: project}
    }

    pub fn lookup_one(&self) -> Result<Option<Entity>> {
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

        let result: Result<(Response, LookupResponse)> = self.ds
            .projects()
            .lookup(req, &self.project)
            .doit();

        match result {
            Ok(res) => {
                let (response, lookup_response): (Response, LookupResponse) = res;
                Ok(DS::unwrap_entity_from_lookup_response(lookup_response))
            },
            Err(e) => {
               Err(e)
            }
        }
    }

    fn unwrap_entity_from_lookup_response (lookup_response:LookupResponse) -> Option<Entity> {
        match lookup_response.found {
            Some (entities) => {
                match entities.first() {
                    Some(_entity) => {
                        _entity.entity.clone()
                    },
                    None => None
                }
            },
            None => None
        }
    }
}
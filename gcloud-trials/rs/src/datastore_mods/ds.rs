// https://github.com/n-k/dstest/blob/master/src/db.rs

use datastore_mods::{
    user::User
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
    ServiceAccountAccess
};

use google_datastore1::{
    Datastore,
    Error,
    CommitRequest,
    Mutation,
    Key,
    CommitResponse
};

pub struct DS {
    pub ds: Datastore<Client, ServiceAccountAccess<Client>>,
    project_name: String
}

impl DS {

    // https://github.com/n-k/dstest
    pub fn new(key_file: String, project_name: &String) -> DS {
        let client_secret = oauth2::service_account_key_from_file(&key_file).unwrap();
        let client = Client::with_connector(HttpsConnector::new(TlsClient::new()));
        let access = ServiceAccountAccess::new(client_secret, client);
        let client = Client::with_connector(HttpsConnector::new(TlsClient::new()));
        let hub = Datastore::new(client, access);
        DS {ds: hub, project_name: project_name.to_string()}
    }

    fn handle_commit_response(&self, result: std::result::Result<(Response, CommitResponse), Error>) -> std::result::Result<Option<Key>, Error> {
        match result {
            Ok((_, commit_response)) => {
                match commit_response.mutation_results {
                    Some(mut_results) => {
                        match mut_results.first() {
                            Some(mr) => Ok(mr.key.clone()),
                            None => Ok(None)
                        }
                    },
                    None => Ok(None)
                }
            },
            Err(e) => {
                Err(e)
            },
        }
    }

    pub fn insert(&self, user: &User) -> std::result::Result<Option<Key>, Error> {
        let request =
            CommitRequest {
                mode: Some("NON_TRANSACTIONAL".to_string()),
                transaction: None,
                mutations: Some(vec![
                    Mutation {
                        insert: Some(user.to_entity()),
                        delete: None,
                        update: None,
                        upsert: None,
                        base_version: None
                    }
                ])
            };

        let result = self.ds
            .projects()
            .commit(request, &self.project_name)
            .doit();

        return self.handle_commit_response(result);
    }

    pub fn delete(&self, user: &User) -> std::result::Result<Option<Key>, Error> {
        let request =
            CommitRequest {
                mode: Some("NON_TRANSACTIONAL".to_string()),
                transaction: None,
                mutations: Some(vec![
                    Mutation {
                        insert: None,
                        delete: Some(user.get_key()),
                        update: None,
                        upsert: None,
                        base_version: None
                    }
                ])
            };

        let result = self.ds
            .projects()
            .commit(request, &self.project_name)
            .doit();

        return self.handle_commit_response(result);
    }

    pub fn update(&self, user: &User) -> std::result::Result<Option<Key>, Error> {
        let request =
            CommitRequest {
                mode: Some("NON_TRANSACTIONAL".to_string()),
                transaction: None,
                mutations: Some(vec![
                    Mutation {
                        insert: None,
                        delete: None,
                        update: Some(user.to_entity()),
                        upsert: None,
                        base_version: None
                    }
                ])
            };

        let result = self.ds
            .projects()
            .commit(request, &self.project_name)
            .doit();

        return self.handle_commit_response(result);
    }

    pub fn upsert(&self, user: &User) -> std::result::Result<Option<Key>, Error> {
        let request =
            CommitRequest {
                mode: Some("NON_TRANSACTIONAL".to_string()),
                transaction: None,
                mutations: Some(vec![
                    Mutation {
                        insert: None,
                        delete: None,
                        update: None,
                        upsert: Some(user.to_entity()),
                        base_version: None
                    }
                ])
            };

        let result = self.ds
            .projects()
            .commit(request, &self.project_name)
            .doit();

        return self.handle_commit_response(result);
    }
}
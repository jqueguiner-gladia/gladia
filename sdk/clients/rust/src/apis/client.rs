use std::rc::Rc;

use hyper;
use super::configuration::Configuration;

pub struct APIClient<C: hyper::client::Connect> {
    configuration: Rc<Configuration<C>>,
    default_api: Box<::apis::DefaultApi>,
    us_api: Box<::apis::UsApi>,
    users_api: Box<::apis::UsersApi>,
}

impl<C: hyper::client::Connect> APIClient<C> {
    pub fn new(configuration: Configuration<C>) -> APIClient<C> {
        let rc = Rc::new(configuration);

        APIClient {
            configuration: rc.clone(),
            default_api: Box::new(::apis::DefaultApiClient::new(rc.clone())),
            us_api: Box::new(::apis::UsApiClient::new(rc.clone())),
            users_api: Box::new(::apis::UsersApiClient::new(rc.clone())),
        }
    }

    pub fn default_api(&self) -> &::apis::DefaultApi{
        self.default_api.as_ref()
    }

    pub fn us_api(&self) -> &::apis::UsApi{
        self.us_api.as_ref()
    }

    pub fn users_api(&self) -> &::apis::UsersApi{
        self.users_api.as_ref()
    }

}

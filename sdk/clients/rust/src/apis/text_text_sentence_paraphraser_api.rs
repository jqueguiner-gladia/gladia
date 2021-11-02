/*
 * FastAPI
 *
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 0.1.0
 * 
 * Generated by: https://openapi-generator.tech
 */

use std::rc::Rc;
use std::borrow::Borrow;

use hyper;
use serde_json;
use futures::Future;

use super::{Error, configuration};
use super::request as __internal_request;

pub struct TextTextSentenceParaphraserApiClient<C: hyper::client::Connect> {
    configuration: Rc<configuration::Configuration<C>>,
}

impl<C: hyper::client::Connect> TextTextSentenceParaphraserApiClient<C> {
    pub fn new(configuration: Rc<configuration::Configuration<C>>) -> TextTextSentenceParaphraserApiClient<C> {
        TextTextSentenceParaphraserApiClient {
            configuration: configuration,
        }
    }
}

pub trait TextTextSentenceParaphraserApi {
    fn apply_text_text_sentence_paraphraser_post(&self, context: &str, model: &str) -> Box<Future<Item = Value, Error = Error<serde_json::Value>>>;
    fn get_versions_text_text_sentence_paraphraser_get(&self, ) -> Box<Future<Item = Value, Error = Error<serde_json::Value>>>;
}


impl<C: hyper::client::Connect>TextTextSentenceParaphraserApi for TextTextSentenceParaphraserApiClient<C> {
    fn apply_text_text_sentence_paraphraser_post(&self, context: &str, model: &str) -> Box<Future<Item = Value, Error = Error<serde_json::Value>>> {
        __internal_request::Request::new(hyper::Method::Post, "/text/text/sentence-paraphraser/".to_string())
            .with_query_param("context".to_string(), context.to_string())
            .with_query_param("model".to_string(), model.to_string())
            .execute(self.configuration.borrow())
    }

    fn get_versions_text_text_sentence_paraphraser_get(&self, ) -> Box<Future<Item = Value, Error = Error<serde_json::Value>>> {
        __internal_request::Request::new(hyper::Method::Get, "/text/text/sentence-paraphraser/".to_string())
            .execute(self.configuration.borrow())
    }

}

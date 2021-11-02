/**
 * FastAPI
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 0.1.0
 * 
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 *
 */


import ApiClient from "../ApiClient";
import HTTPValidationError from '../model/HTTPValidationError';

/**
* TextTextHateSpeechDetection service.
* @module api/TextTextHateSpeechDetectionApi
* @version 0.1.0
*/
export default class TextTextHateSpeechDetectionApi {

    /**
    * Constructs a new TextTextHateSpeechDetectionApi. 
    * @alias module:api/TextTextHateSpeechDetectionApi
    * @class
    * @param {module:ApiClient} [apiClient] Optional API client implementation to use,
    * default to {@link module:ApiClient#instance} if unspecified.
    */
    constructor(apiClient) {
        this.apiClient = apiClient || ApiClient.instance;
    }


    /**
     * Callback function to receive the result of the applyTextTextHateSpeechDetectionPost operation.
     * @callback module:api/TextTextHateSpeechDetectionApi~applyTextTextHateSpeechDetectionPostCallback
     * @param {String} error Error message, if any.
     * @param {Object} data The data returned by the service call.
     * @param {String} response The complete HTTP response.
     */

    /**
     * Apply model for the hate-speech-detection task for a given models
     * @param {Object} opts Optional parameters
     * @param {String} opts.text  (default to 'I hate you piece of shit')
     * @param {String} opts.model  (default to 'Hate-speech-CNERG-dehatebert-mono-english')
     * @param {module:api/TextTextHateSpeechDetectionApi~applyTextTextHateSpeechDetectionPostCallback} callback The callback function, accepting three arguments: error, data, response
     * data is of type: {@link Object}
     */
    applyTextTextHateSpeechDetectionPost(opts, callback) {
      opts = opts || {};
      let postBody = null;

      let pathParams = {
      };
      let queryParams = {
        'text': opts['text'],
        'model': opts['model']
      };
      let headerParams = {
      };
      let formParams = {
      };

      let authNames = [];
      let contentTypes = [];
      let accepts = ['application/json'];
      let returnType = Object;
      return this.apiClient.callApi(
        '/text/text/hate-speech-detection/', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType, null, callback
      );
    }

    /**
     * Callback function to receive the result of the getVersionsTextTextHateSpeechDetectionGet operation.
     * @callback module:api/TextTextHateSpeechDetectionApi~getVersionsTextTextHateSpeechDetectionGetCallback
     * @param {String} error Error message, if any.
     * @param {Object} data The data returned by the service call.
     * @param {String} response The complete HTTP response.
     */

    /**
     * Get list of models available for hate-speech-detection
     * @param {module:api/TextTextHateSpeechDetectionApi~getVersionsTextTextHateSpeechDetectionGetCallback} callback The callback function, accepting three arguments: error, data, response
     * data is of type: {@link Object}
     */
    getVersionsTextTextHateSpeechDetectionGet(callback) {
      let postBody = null;

      let pathParams = {
      };
      let queryParams = {
      };
      let headerParams = {
      };
      let formParams = {
      };

      let authNames = [];
      let contentTypes = [];
      let accepts = ['application/json'];
      let returnType = Object;
      return this.apiClient.callApi(
        '/text/text/hate-speech-detection/', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType, null, callback
      );
    }


}

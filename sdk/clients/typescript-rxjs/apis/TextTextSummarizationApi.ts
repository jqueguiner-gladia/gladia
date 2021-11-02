// tslint:disable
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
 */

import { Observable } from 'rxjs';
import { BaseAPI, RequiredError, HttpHeaders, HttpQuery, COLLECTION_FORMATS } from '../runtime';
import {
    HTTPValidationError,
} from '../models';

export interface ApplyTextTextSummarizationPostRequest {
    text?: string;
    sourceLanguage?: string;
    maxLength?: number;
    minLength?: number;
    model?: ApplyTextTextSummarizationPostModelEnum;
}

/**
 * no description
 */
export class TextTextSummarizationApi extends BaseAPI {

    /**
     * Apply model for the summarization task for a given models
     */
    applyTextTextSummarizationPost(requestParameters: ApplyTextTextSummarizationPostRequest): Observable<object> {
        const queryParameters: HttpQuery = {};

        if (requestParameters.text !== undefined && requestParameters.text !== null) {
            queryParameters['text'] = requestParameters.text;
        }

        if (requestParameters.sourceLanguage !== undefined && requestParameters.sourceLanguage !== null) {
            queryParameters['source_language'] = requestParameters.sourceLanguage;
        }

        if (requestParameters.maxLength !== undefined && requestParameters.maxLength !== null) {
            queryParameters['max_length'] = requestParameters.maxLength;
        }

        if (requestParameters.minLength !== undefined && requestParameters.minLength !== null) {
            queryParameters['min_length'] = requestParameters.minLength;
        }

        if (requestParameters.model !== undefined && requestParameters.model !== null) {
            queryParameters['model'] = requestParameters.model;
        }

        const headerParameters: HttpHeaders = {};

        return this.request<object>({
            path: `/text/text/summarization/`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
        });
    }

    /**
     * Get list of models available for summarization
     */
    getVersionsTextTextSummarizationGet(): Observable<object> {
        const queryParameters: HttpQuery = {};

        const headerParameters: HttpHeaders = {};

        return this.request<object>({
            path: `/text/text/summarization/`,
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        });
    }

}

/**
    * @export
    * @enum {string}
    */
export enum ApplyTextTextSummarizationPostModelEnum {
    DistilbartCnn126 = 'distilbart-cnn-12-6'
}

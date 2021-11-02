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

export interface ApplyTextTextAutocorrectPostRequest {
    sentence?: string;
    model?: ApplyTextTextAutocorrectPostModelEnum;
}

/**
 * no description
 */
export class TextTextAutocorrectApi extends BaseAPI {

    /**
     * Apply model for the autocorrect task for a given models
     */
    applyTextTextAutocorrectPost(requestParameters: ApplyTextTextAutocorrectPostRequest): Observable<object> {
        const queryParameters: HttpQuery = {};

        if (requestParameters.sentence !== undefined && requestParameters.sentence !== null) {
            queryParameters['sentence'] = requestParameters.sentence;
        }

        if (requestParameters.model !== undefined && requestParameters.model !== null) {
            queryParameters['model'] = requestParameters.model;
        }

        const headerParameters: HttpHeaders = {};

        return this.request<object>({
            path: `/text/text/autocorrect/`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
        });
    }

    /**
     * Get list of models available for autocorrect
     */
    getVersionsTextTextAutocorrectGet(): Observable<object> {
        const queryParameters: HttpQuery = {};

        const headerParameters: HttpHeaders = {};

        return this.request<object>({
            path: `/text/text/autocorrect/`,
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
export enum ApplyTextTextAutocorrectPostModelEnum {
    FlexudyT5BaseMultiSentenceDoctor = 'flexudy-t5-base-multi-sentence-doctor'
}

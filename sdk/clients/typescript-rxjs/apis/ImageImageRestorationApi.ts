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

export interface ApplyImageImageRestorationPostRequest {
    image: Blob;
    model?: string;
}

/**
 * no description
 */
export class ImageImageRestorationApi extends BaseAPI {

    /**
     * Apply model for the restoration task for a given models
     */
    applyImageImageRestorationPost(requestParameters: ApplyImageImageRestorationPostRequest): Observable<object> {
        if (requestParameters.image === null || requestParameters.image === undefined) {
            throw new RequiredError('image','Required parameter requestParameters.image was null or undefined when calling applyImageImageRestorationPost.');
        }

        const queryParameters: HttpQuery = {};

        if (requestParameters.model !== undefined && requestParameters.model !== null) {
            queryParameters['model'] = requestParameters.model;
        }

        const headerParameters: HttpHeaders = {};

        const formData = new FormData();
        if (requestParameters.image !== undefined) {
            formData.append('image', requestParameters.image as any);
        }

        return this.request<object>({
            path: `/image/image/restoration/`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
            body: formData,
        });
    }

    /**
     * Get list of models available for restoration
     */
    getVersionsImageImageRestorationGet(): Observable<object> {
        const queryParameters: HttpQuery = {};

        const headerParameters: HttpHeaders = {};

        return this.request<object>({
            path: `/image/image/restoration/`,
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        });
    }

}

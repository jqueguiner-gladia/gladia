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
/* tslint:disable:no-unused-variable member-ordering */

import { Observable } from "rxjs/Observable";
import 'rxjs/add/operator/map';
import 'rxjs/add/operator/toPromise';
import IHttpClient from "../IHttpClient";
import { inject, injectable } from "inversify";
import { IAPIConfiguration } from "../IAPIConfiguration";
import { Headers } from "../Headers";
import HttpResponse from "../HttpResponse";

import { HTTPValidationError } from '../model/hTTPValidationError';

import { COLLECTION_FORMATS }  from '../variables';



@injectable()
export class TextTextLanguageDetectionService {
    private basePath: string = 'http://localhost';

    constructor(@inject("IApiHttpClient") private httpClient: IHttpClient,
        @inject("IAPIConfiguration") private APIConfiguration: IAPIConfiguration ) {
        if(this.APIConfiguration.basePath)
            this.basePath = this.APIConfiguration.basePath;
    }

    /**
     * Apply model for the language-detection task for a given models
     * 
     * @param text 
     * @param model 
     
     */
    public applyTextTextLanguageDetectionPost(text?: string, model?: 'toftrup-etal-2021', observe?: 'body', headers?: Headers): Observable<object>;
    public applyTextTextLanguageDetectionPost(text?: string, model?: 'toftrup-etal-2021', observe?: 'response', headers?: Headers): Observable<HttpResponse<object>>;
    public applyTextTextLanguageDetectionPost(text?: string, model?: 'toftrup-etal-2021', observe: any = 'body', headers: Headers = {}): Observable<any> {
        let queryParameters: string[] = [];
        if (text !== undefined) {
            queryParameters.push("text="+encodeURIComponent(String(text)));
        }
        if (model !== undefined) {
            queryParameters.push("model="+encodeURIComponent(String(model)));
        }

        headers['Accept'] = 'application/json';

        const response: Observable<HttpResponse<object>> = this.httpClient.post(`${this.basePath}/text/text/language-detection/?${queryParameters.join('&')}`, headers);
        if (observe == 'body') {
               return response.map(httpResponse => <object>(httpResponse.response));
        }
        return response;
    }


    /**
     * Get list of models available for language-detection
     * 
     
     */
    public getVersionsTextTextLanguageDetectionGet(observe?: 'body', headers?: Headers): Observable<object>;
    public getVersionsTextTextLanguageDetectionGet(observe?: 'response', headers?: Headers): Observable<HttpResponse<object>>;
    public getVersionsTextTextLanguageDetectionGet(observe: any = 'body', headers: Headers = {}): Observable<any> {
        headers['Accept'] = 'application/json';

        const response: Observable<HttpResponse<object>> = this.httpClient.get(`${this.basePath}/text/text/language-detection/`, headers);
        if (observe == 'body') {
               return response.map(httpResponse => <object>(httpResponse.response));
        }
        return response;
    }

}

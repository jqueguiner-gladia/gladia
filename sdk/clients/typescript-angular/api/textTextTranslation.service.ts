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

import { Inject, Injectable, Optional }                      from '@angular/core';
import { HttpClient, HttpHeaders, HttpParams,
         HttpResponse, HttpEvent }                           from '@angular/common/http';
import { CustomHttpUrlEncodingCodec }                        from '../encoder';

import { Observable }                                        from 'rxjs';

import { HTTPValidationError } from '../model/hTTPValidationError';

import { BASE_PATH, COLLECTION_FORMATS }                     from '../variables';
import { Configuration }                                     from '../configuration';


@Injectable({
  providedIn: 'root'
})
export class TextTextTranslationService {

    protected basePath = 'http://localhost';
    public defaultHeaders = new HttpHeaders();
    public configuration = new Configuration();

    constructor(protected httpClient: HttpClient, @Optional()@Inject(BASE_PATH) basePath: string, @Optional() configuration: Configuration) {

        if (configuration) {
            this.configuration = configuration;
            this.configuration.basePath = configuration.basePath || basePath || this.basePath;

        } else {
            this.configuration.basePath = basePath || this.basePath;
        }
    }

    /**
     * @param consumes string[] mime-types
     * @return true: consumes contains 'multipart/form-data', false: otherwise
     */
    private canConsumeForm(consumes: string[]): boolean {
        const form = 'multipart/form-data';
        for (const consume of consumes) {
            if (form === consume) {
                return true;
            }
        }
        return false;
    }


    /**
     * Apply model for the translation task for a given models
     * 
     * @param inputString 
     * @param sourceLanguage 
     * @param targetLanguage 
     * @param model 
     * @param observe set whether or not to return the data Observable as the body, response or events. defaults to returning the body.
     * @param reportProgress flag to report request and response progress.
     */
    public applyTextTextTranslationPost(inputString?: string, sourceLanguage?: string, targetLanguage?: string, model?: 'Helsinki-NLP', observe?: 'body', reportProgress?: boolean): Observable<object>;
    public applyTextTextTranslationPost(inputString?: string, sourceLanguage?: string, targetLanguage?: string, model?: 'Helsinki-NLP', observe?: 'response', reportProgress?: boolean): Observable<HttpResponse<object>>;
    public applyTextTextTranslationPost(inputString?: string, sourceLanguage?: string, targetLanguage?: string, model?: 'Helsinki-NLP', observe?: 'events', reportProgress?: boolean): Observable<HttpEvent<object>>;
    public applyTextTextTranslationPost(inputString?: string, sourceLanguage?: string, targetLanguage?: string, model?: 'Helsinki-NLP', observe: any = 'body', reportProgress: boolean = false ): Observable<any> {

        let queryParameters = new HttpParams({encoder: new CustomHttpUrlEncodingCodec()});
        if (inputString !== undefined && inputString !== null) {
            queryParameters = queryParameters.set('input_string', <any>inputString);
        }
        if (sourceLanguage !== undefined && sourceLanguage !== null) {
            queryParameters = queryParameters.set('source_language', <any>sourceLanguage);
        }
        if (targetLanguage !== undefined && targetLanguage !== null) {
            queryParameters = queryParameters.set('target_language', <any>targetLanguage);
        }
        if (model !== undefined && model !== null) {
            queryParameters = queryParameters.set('model', <any>model);
        }

        let headers = this.defaultHeaders;

        // to determine the Accept header
        const httpHeaderAccepts: string[] = [
            'application/json'
        ];
        const httpHeaderAcceptSelected: string | undefined = this.configuration.selectHeaderAccept(httpHeaderAccepts);
        if (httpHeaderAcceptSelected !== undefined) {
            headers = headers.set('Accept', httpHeaderAcceptSelected);
        }

        // to determine the Content-Type header
        const consumes: string[] = [
        ];

        return this.httpClient.post<object>(`${this.configuration.basePath}/text/text/translation/`,
            null,
            {
                params: queryParameters,
                withCredentials: this.configuration.withCredentials,
                headers: headers,
                observe: observe,
                reportProgress: reportProgress
            }
        );
    }

    /**
     * Get list of models available for translation
     * 
     * @param observe set whether or not to return the data Observable as the body, response or events. defaults to returning the body.
     * @param reportProgress flag to report request and response progress.
     */
    public getVersionsTextTextTranslationGet(observe?: 'body', reportProgress?: boolean): Observable<object>;
    public getVersionsTextTextTranslationGet(observe?: 'response', reportProgress?: boolean): Observable<HttpResponse<object>>;
    public getVersionsTextTextTranslationGet(observe?: 'events', reportProgress?: boolean): Observable<HttpEvent<object>>;
    public getVersionsTextTextTranslationGet(observe: any = 'body', reportProgress: boolean = false ): Observable<any> {

        let headers = this.defaultHeaders;

        // to determine the Accept header
        const httpHeaderAccepts: string[] = [
            'application/json'
        ];
        const httpHeaderAcceptSelected: string | undefined = this.configuration.selectHeaderAccept(httpHeaderAccepts);
        if (httpHeaderAcceptSelected !== undefined) {
            headers = headers.set('Accept', httpHeaderAcceptSelected);
        }

        // to determine the Content-Type header
        const consumes: string[] = [
        ];

        return this.httpClient.get<object>(`${this.configuration.basePath}/text/text/translation/`,
            {
                withCredentials: this.configuration.withCredentials,
                headers: headers,
                observe: observe,
                reportProgress: reportProgress
            }
        );
    }

}

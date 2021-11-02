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

import * as models from '../model/models';

/* tslint:disable:no-unused-variable member-ordering */

export class TextTextNextWordPredictionApi {
    protected basePath = 'http://localhost';
    public defaultHeaders : any = {};

    static $inject: string[] = ['$http', '$httpParamSerializer', 'basePath'];

    constructor(protected $http: ng.IHttpService, protected $httpParamSerializer?: (d: any) => any, basePath?: string) {
        if (basePath !== undefined) {
            this.basePath = basePath;
        }
    }

    /**
     * 
     * @summary Apply model for the next-word-prediction task for a given models
     * @param sentence 
     * @param model 
     */
    public applyTextTextNextWordPredictionPost (sentence?: string, model?: 'bert-base-uncased' | 'roberta-base' | 'distilbert-base-uncased' | 'albert-base-v2', extraHttpRequestParams?: any ) : ng.IHttpPromise<object> {
        const localVarPath = this.basePath + '/text/text/next-word-prediction/';

        let queryParameters: any = {};
        let headerParams: any = (<any>Object).assign({}, this.defaultHeaders);
        if (sentence !== undefined) {
            queryParameters['sentence'] = sentence;
        }

        if (model !== undefined) {
            queryParameters['model'] = model;
        }

        let httpRequestParams: ng.IRequestConfig = {
            method: 'POST',
            url: localVarPath,
            params: queryParameters,
            headers: headerParams
        };

        if (extraHttpRequestParams) {
            httpRequestParams = (<any>Object).assign(httpRequestParams, extraHttpRequestParams);
        }

        return this.$http(httpRequestParams);
    }
    /**
     * 
     * @summary Get list of models available for next-word-prediction
     */
    public getVersionsTextTextNextWordPredictionGet (extraHttpRequestParams?: any ) : ng.IHttpPromise<object> {
        const localVarPath = this.basePath + '/text/text/next-word-prediction/';

        let queryParameters: any = {};
        let headerParams: any = (<any>Object).assign({}, this.defaultHeaders);
        let httpRequestParams: ng.IRequestConfig = {
            method: 'GET',
            url: localVarPath,
            params: queryParameters,
            headers: headerParams
        };

        if (extraHttpRequestParams) {
            httpRequestParams = (<any>Object).assign(httpRequestParams, extraHttpRequestParams);
        }

        return this.$http(httpRequestParams);
    }
}

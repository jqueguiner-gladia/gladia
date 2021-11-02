package org.openapitools.api;

import org.openapitools.api.ApiUtils
import org.openapitools.model.HTTPValidationError

class TextTextNextSentencePredictionApi {
    String basePath = "http://localhost"
    String versionPath = ""
    ApiUtils apiUtils = new ApiUtils();

    def applyTextTextNextSentencePredictionPost ( String sentence1, String sentence2, String model, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/text/text/next-sentence-prediction/"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        

        if (sentence1 != null) {
            queryParams.put("sentence_1", sentence1)
        }
        if (sentence2 != null) {
            queryParams.put("sentence_2", sentence2)
        }
        if (model != null) {
            queryParams.put("model", model)
        }




        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "POST", "",
                    Object.class )

    }

    def getVersionsTextTextNextSentencePredictionGet ( Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/text/text/next-sentence-prediction/"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        





        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "GET", "",
                    Object.class )

    }

}

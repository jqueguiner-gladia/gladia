package org.openapitools.api;

import org.openapitools.api.ApiUtils

class DefaultApi {
    String basePath = "http://localhost"
    String versionPath = ""
    ApiUtils apiUtils = new ApiUtils();

    def readUsersImageImageUncolorizationUsersGet ( Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/image/image/uncolorization/users/"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        





        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "GET", "",
                    Object.class )

    }

    def rootGet ( Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/"

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

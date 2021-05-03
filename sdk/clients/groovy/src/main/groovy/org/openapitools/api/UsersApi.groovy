package org.openapitools.api;

import org.openapitools.api.ApiUtils

class UsersApi {
    String basePath = "http://localhost"
    String versionPath = ""
    ApiUtils apiUtils = new ApiUtils();

    def readUserMeImageImageUncolorizationUsersMeGet ( Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/image/image/uncolorization/users/me"

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

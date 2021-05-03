package org.openapitools.api;

import org.openapitools.api.ApiUtils
import org.openapitools.model.HTTPValidationError

class UsApi {
    String basePath = "http://localhost"
    String versionPath = ""
    ApiUtils apiUtils = new ApiUtils();

    def readUserImageImageUncolorizationUsersUsernamePost ( String username, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/image/image/uncolorization/users/${username}"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        
        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }
        





        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "POST", "",
                    Object.class )

    }

}

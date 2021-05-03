package org.openapitools.client.api {

import org.openapitools.common.ApiInvoker;
import org.openapitools.exception.ApiErrorCodes;
import org.openapitools.exception.ApiError;
import org.openapitools.common.ApiUserCredentials;
import org.openapitools.event.Response;
import org.openapitools.common.OpenApi;
import org.openapitools.client.model.HTTPValidationError;
import org.openapitools.client.model.Object;

import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class UsApi extends OpenApi {
    /**
    * Constructor for the UsApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function UsApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

        public static const event_read_user_image_image_uncolorization_users_username_post: String = "read_user_image_image_uncolorization_users_username_post";


    /*
     * Returns Object 
     */
    public function read_user_image_image_uncolorization_users_username_post (username: String): String {
        // create path and map variables
        var path: String = "/image/image/uncolorization/users/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "read_user_image_image_uncolorization_users_username_post";

        token.returnType = Object;
        return requestId;

    }
}
}

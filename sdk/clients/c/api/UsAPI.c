#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "UsAPI.h"


#define MAX_BUFFER_LENGTH 4096
#define intToStr(dst, src) \
    do {\
    char dst[256];\
    snprintf(dst, 256, "%ld", (long int)(src));\
}while(0)

// Read User
//
object_t*
UsAPI_readUserImageImageUncolorizationUsersUsernamePost(apiClient_t *apiClient ,char * username)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/image/image/uncolorization/users/{username}")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/image/image/uncolorization/users/{username}");


    // Path Params
    long sizeOfPathParams_username = strlen(username)+3 + strlen("{ username }");
    if(username == NULL) {
        goto end;
    }
    char* localVarToReplace_username = malloc(sizeOfPathParams_username);
    sprintf(localVarToReplace_username, "{%s}", "username");

    localVarPath = strReplace(localVarPath, localVarToReplace_username, username);


    list_addElement(localVarHeaderType,"application/json"); //produces
    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "POST");

    if (apiClient->response_code == 200) {
        printf("%s\n","Successful Response");
    }
    if (apiClient->response_code == 422) {
        printf("%s\n","Validation Error");
    }
    //nonprimitive not container
    cJSON *UsAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
    object_t *elementToReturn = object_parseFromJSON(UsAPIlocalVarJSON);
    cJSON_Delete(UsAPIlocalVarJSON);
    if(elementToReturn == NULL) {
        // return 0;
    }

    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    
    
    
    list_free(localVarHeaderType);
    
    free(localVarPath);
    free(localVarToReplace_username);
    return elementToReturn;
end:
    return NULL;

}


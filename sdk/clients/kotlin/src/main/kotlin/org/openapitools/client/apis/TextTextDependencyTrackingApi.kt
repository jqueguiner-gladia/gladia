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
package org.openapitools.client.apis

import org.openapitools.client.models.HTTPValidationError

import org.openapitools.client.infrastructure.ApiClient
import org.openapitools.client.infrastructure.ClientException
import org.openapitools.client.infrastructure.ClientError
import org.openapitools.client.infrastructure.ServerException
import org.openapitools.client.infrastructure.ServerError
import org.openapitools.client.infrastructure.MultiValueMap
import org.openapitools.client.infrastructure.RequestConfig
import org.openapitools.client.infrastructure.RequestMethod
import org.openapitools.client.infrastructure.ResponseType
import org.openapitools.client.infrastructure.Success
import org.openapitools.client.infrastructure.toMultiValue

class TextTextDependencyTrackingApi(basePath: kotlin.String = "http://localhost") : ApiClient(basePath) {

    /**
    * Apply model for the dependency-tracking task for a given models
    * 
    * @param inputString  (optional, default to 'Text to analyzed')
    * @param model  (optional, default to 'LAL-Parser')
    * @return kotlin.Any
    */
    @Suppress("UNCHECKED_CAST")
    fun applyTextTextDependencyTrackingPost(inputString: kotlin.String?, model: kotlin.String?) : kotlin.Any {
        val localVariableBody: kotlin.Any? = null
        val localVariableQuery: MultiValueMap = mapOf("inputString" to listOf("$inputString"), "model" to listOf("$model"))
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf()
        val localVariableConfig = RequestConfig(
            RequestMethod.POST,
            "/text/text/dependency-tracking/",
            query = localVariableQuery,
            headers = localVariableHeaders
        )
        val response = request<kotlin.Any>(
            localVariableConfig,
            localVariableBody
        )

        return when (response.responseType) {
            ResponseType.Success -> (response as Success<*>).data as kotlin.Any
            ResponseType.Informational -> TODO()
            ResponseType.Redirection -> TODO()
            ResponseType.ClientError -> throw ClientException((response as ClientError<*>).body as? String ?: "Client error")
            ResponseType.ServerError -> throw ServerException((response as ServerError<*>).message ?: "Server error")
        }
    }

    /**
    * Get list of models available for dependency-tracking
    * 
    * @return kotlin.Any
    */
    @Suppress("UNCHECKED_CAST")
    fun getVersionsTextTextDependencyTrackingGet() : kotlin.Any {
        val localVariableBody: kotlin.Any? = null
        val localVariableQuery: MultiValueMap = mapOf()
        val localVariableHeaders: MutableMap<String, String> = mutableMapOf()
        val localVariableConfig = RequestConfig(
            RequestMethod.GET,
            "/text/text/dependency-tracking/",
            query = localVariableQuery,
            headers = localVariableHeaders
        )
        val response = request<kotlin.Any>(
            localVariableConfig,
            localVariableBody
        )

        return when (response.responseType) {
            ResponseType.Success -> (response as Success<*>).data as kotlin.Any
            ResponseType.Informational -> TODO()
            ResponseType.Redirection -> TODO()
            ResponseType.ClientError -> throw ClientException((response as ClientError<*>).body as? String ?: "Client error")
            ResponseType.ServerError -> throw ServerException((response as ServerError<*>).message ?: "Server error")
        }
    }

}

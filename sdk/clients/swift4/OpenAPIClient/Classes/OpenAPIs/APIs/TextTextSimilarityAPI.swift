//
// TextTextSimilarityAPI.swift
//
// Generated by openapi-generator
// https://openapi-generator.tech
//

import Foundation
import Alamofire



open class TextTextSimilarityAPI {
    /**
     * enum for parameter model
     */
    public enum Model_applyTextTextSimilarityPost: String {
        case allMinilmL6V2 = "all-MiniLM-L6-v2"
    }

    /**
     Apply model for the similarity task for a given models
     
     - parameter sentence1: (query)  (optional, default to "I like Python because I can build AI applications")
     - parameter sentence2: (query)  (optional, default to "Second sentence to compare to")
     - parameter model: (query)  (optional, default to .all-MiniLM-L6-v2)
     - parameter completion: completion handler to receive the data and the error objects
     */
    open class func applyTextTextSimilarityPost(sentence1: String? = nil, sentence2: String? = nil, model: Model_applyTextTextSimilarityPost? = nil, completion: @escaping ((_ data: Any?,_ error: Error?) -> Void)) {
        applyTextTextSimilarityPostWithRequestBuilder(sentence1: sentence1, sentence2: sentence2, model: model).execute { (response, error) -> Void in
            completion(response?.body, error)
        }
    }

    /**
     Apply model for the similarity task for a given models
     - POST /text/text/similarity/
     - parameter sentence1: (query)  (optional, default to "I like Python because I can build AI applications")
     - parameter sentence2: (query)  (optional, default to "Second sentence to compare to")
     - parameter model: (query)  (optional, default to .all-MiniLM-L6-v2)
     - returns: RequestBuilder<Any> 
     */
    open class func applyTextTextSimilarityPostWithRequestBuilder(sentence1: String? = nil, sentence2: String? = nil, model: Model_applyTextTextSimilarityPost? = nil) -> RequestBuilder<Any> {
        let path = "/text/text/similarity/"
        let URLString = OpenAPIClientAPI.basePath + path
        let parameters: [String:Any]? = nil
        
        var url = URLComponents(string: URLString)
        url?.queryItems = APIHelper.mapValuesToQueryItems([
            "sentence_1": sentence1, 
            "sentence_2": sentence2, 
            "model": model?.rawValue
        ])

        let requestBuilder: RequestBuilder<Any>.Type = OpenAPIClientAPI.requestBuilderFactory.getBuilder()

        return requestBuilder.init(method: "POST", URLString: (url?.string ?? URLString), parameters: parameters, isBody: false)
    }

    /**
     Get list of models available for similarity
     
     - parameter completion: completion handler to receive the data and the error objects
     */
    open class func getVersionsTextTextSimilarityGet(completion: @escaping ((_ data: Any?,_ error: Error?) -> Void)) {
        getVersionsTextTextSimilarityGetWithRequestBuilder().execute { (response, error) -> Void in
            completion(response?.body, error)
        }
    }

    /**
     Get list of models available for similarity
     - GET /text/text/similarity/
     - returns: RequestBuilder<Any> 
     */
    open class func getVersionsTextTextSimilarityGetWithRequestBuilder() -> RequestBuilder<Any> {
        let path = "/text/text/similarity/"
        let URLString = OpenAPIClientAPI.basePath + path
        let parameters: [String:Any]? = nil
        
        let url = URLComponents(string: URLString)

        let requestBuilder: RequestBuilder<Any>.Type = OpenAPIClientAPI.requestBuilderFactory.getBuilder()

        return requestBuilder.init(method: "GET", URLString: (url?.string ?? URLString), parameters: parameters, isBody: false)
    }

}

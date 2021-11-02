//
// TextTextEntityExtractionAPI.swift
//
// Generated by openapi-generator
// https://openapi-generator.tech
//

import Alamofire



public class TextTextEntityExtractionAPI: APIBase {
    /**
     Apply model for the entity-extraction task for a given models
     
     - parameter inputString: (query)  (optional)
     - parameter model: (query)  (optional)
     - parameter completion: completion handler to receive the data and the error objects
     */
    public class func applyTextTextEntityExtractionPost(inputString inputString: String? = nil, model: String? = nil, completion: ((data: AnyObject?, error: ErrorType?) -> Void)) {
        applyTextTextEntityExtractionPostWithRequestBuilder(inputString: inputString, model: model).execute { (response, error) -> Void in
            completion(data: response?.body, error: error);
        }
    }


    /**
     Apply model for the entity-extraction task for a given models
     - POST /text/text/entity-extraction/     - parameter inputString: (query)  (optional)
     - parameter model: (query)  (optional)

     - returns: RequestBuilder<AnyObject> 
     */
    public class func applyTextTextEntityExtractionPostWithRequestBuilder(inputString inputString: String? = nil, model: String? = nil) -> RequestBuilder<AnyObject> {
        let path = "/text/text/entity-extraction/"
        let URLString = OpenAPIClientAPI.basePath + path

        let nillableParameters: [String:AnyObject?] = [
            "input_string": inputString,
            "model": model
        ]
 
        let parameters = APIHelper.rejectNil(nillableParameters)
 
        let convertedParameters = APIHelper.convertBoolToString(parameters)
 
        let requestBuilder: RequestBuilder<AnyObject>.Type = OpenAPIClientAPI.requestBuilderFactory.getBuilder()

        return requestBuilder.init(method: "POST", URLString: URLString, parameters: convertedParameters, isBody: false)
    }

    /**
     Get list of models available for entity-extraction
     
     - parameter completion: completion handler to receive the data and the error objects
     */
    public class func getVersionsTextTextEntityExtractionGet(completion: ((data: AnyObject?, error: ErrorType?) -> Void)) {
        getVersionsTextTextEntityExtractionGetWithRequestBuilder().execute { (response, error) -> Void in
            completion(data: response?.body, error: error);
        }
    }


    /**
     Get list of models available for entity-extraction
     - GET /text/text/entity-extraction/
     - returns: RequestBuilder<AnyObject> 
     */
    public class func getVersionsTextTextEntityExtractionGetWithRequestBuilder() -> RequestBuilder<AnyObject> {
        let path = "/text/text/entity-extraction/"
        let URLString = OpenAPIClientAPI.basePath + path

        let nillableParameters: [String:AnyObject?] = [:]
 
        let parameters = APIHelper.rejectNil(nillableParameters)
 
        let convertedParameters = APIHelper.convertBoolToString(parameters)
 
        let requestBuilder: RequestBuilder<AnyObject>.Type = OpenAPIClientAPI.requestBuilderFactory.getBuilder()

        return requestBuilder.init(method: "GET", URLString: URLString, parameters: convertedParameters, isBody: true)
    }

}

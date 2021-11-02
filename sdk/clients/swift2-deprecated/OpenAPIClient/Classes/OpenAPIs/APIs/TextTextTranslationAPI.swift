//
// TextTextTranslationAPI.swift
//
// Generated by openapi-generator
// https://openapi-generator.tech
//

import Alamofire



public class TextTextTranslationAPI: APIBase {
    /**
     * enum for parameter model
     */
    public enum Model_applyTextTextTranslationPost: String { 
        case HelsinkiNlp = "Helsinki-NLP"
    }

    /**
     Apply model for the translation task for a given models
     
     - parameter inputString: (query)  (optional)
     - parameter sourceLanguage: (query)  (optional)
     - parameter targetLanguage: (query)  (optional)
     - parameter model: (query)  (optional)
     - parameter completion: completion handler to receive the data and the error objects
     */
    public class func applyTextTextTranslationPost(inputString inputString: String? = nil, sourceLanguage: String? = nil, targetLanguage: String? = nil, model: Model_applyTextTextTranslationPost? = nil, completion: ((data: AnyObject?, error: ErrorType?) -> Void)) {
        applyTextTextTranslationPostWithRequestBuilder(inputString: inputString, sourceLanguage: sourceLanguage, targetLanguage: targetLanguage, model: model).execute { (response, error) -> Void in
            completion(data: response?.body, error: error);
        }
    }


    /**
     Apply model for the translation task for a given models
     - POST /text/text/translation/     - parameter inputString: (query)  (optional)
     - parameter sourceLanguage: (query)  (optional)
     - parameter targetLanguage: (query)  (optional)
     - parameter model: (query)  (optional)

     - returns: RequestBuilder<AnyObject> 
     */
    public class func applyTextTextTranslationPostWithRequestBuilder(inputString inputString: String? = nil, sourceLanguage: String? = nil, targetLanguage: String? = nil, model: Model_applyTextTextTranslationPost? = nil) -> RequestBuilder<AnyObject> {
        let path = "/text/text/translation/"
        let URLString = OpenAPIClientAPI.basePath + path

        let nillableParameters: [String:AnyObject?] = [
            "input_string": inputString,
            "source_language": sourceLanguage,
            "target_language": targetLanguage,
            "model": model?.rawValue
        ]
 
        let parameters = APIHelper.rejectNil(nillableParameters)
 
        let convertedParameters = APIHelper.convertBoolToString(parameters)
 
        let requestBuilder: RequestBuilder<AnyObject>.Type = OpenAPIClientAPI.requestBuilderFactory.getBuilder()

        return requestBuilder.init(method: "POST", URLString: URLString, parameters: convertedParameters, isBody: false)
    }

    /**
     Get list of models available for translation
     
     - parameter completion: completion handler to receive the data and the error objects
     */
    public class func getVersionsTextTextTranslationGet(completion: ((data: AnyObject?, error: ErrorType?) -> Void)) {
        getVersionsTextTextTranslationGetWithRequestBuilder().execute { (response, error) -> Void in
            completion(data: response?.body, error: error);
        }
    }


    /**
     Get list of models available for translation
     - GET /text/text/translation/
     - returns: RequestBuilder<AnyObject> 
     */
    public class func getVersionsTextTextTranslationGetWithRequestBuilder() -> RequestBuilder<AnyObject> {
        let path = "/text/text/translation/"
        let URLString = OpenAPIClientAPI.basePath + path

        let nillableParameters: [String:AnyObject?] = [:]
 
        let parameters = APIHelper.rejectNil(nillableParameters)
 
        let convertedParameters = APIHelper.convertBoolToString(parameters)
 
        let requestBuilder: RequestBuilder<AnyObject>.Type = OpenAPIClientAPI.requestBuilderFactory.getBuilder()

        return requestBuilder.init(method: "GET", URLString: URLString, parameters: convertedParameters, isBody: true)
    }

}

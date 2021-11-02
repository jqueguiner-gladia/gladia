#import <Foundation/Foundation.h>
#import "OAIHTTPValidationError.h"
#import "OAIApi.h"

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



@interface OAITextTextTranslationApi: NSObject <OAIApi>

extern NSString* kOAITextTextTranslationApiErrorDomain;
extern NSInteger kOAITextTextTranslationApiMissingParamErrorCode;

-(instancetype) initWithApiClient:(OAIApiClient *)apiClient NS_DESIGNATED_INITIALIZER;

/// Apply model for the translation task for a given models
/// 
///
/// @param inputString  (optional) (default to @"Text to translate")
/// @param sourceLanguage  (optional) (default to @"en")
/// @param targetLanguage  (optional) (default to @"fr")
/// @param model  (optional) (default to @"Helsinki-NLP")
/// 
///  code:200 message:"Successful Response",
///  code:422 message:"Validation Error"
///
/// @return NSObject*
-(NSURLSessionTask*) applyTextTextTranslationPostWithInputString: (NSString*) inputString
    sourceLanguage: (NSString*) sourceLanguage
    targetLanguage: (NSString*) targetLanguage
    model: (NSString*) model
    completionHandler: (void (^)(NSObject* output, NSError* error)) handler;


/// Get list of models available for translation
/// 
///
/// 
///  code:200 message:"Successful Response"
///
/// @return NSObject*
-(NSURLSessionTask*) getVersionsTextTextTranslationGetWithCompletionHandler: 
    (void (^)(NSObject* output, NSError* error)) handler;



@end

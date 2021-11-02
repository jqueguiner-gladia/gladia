//
// HTTPValidationError.swift
//
// Generated by openapi-generator
// https://openapi-generator.tech
//

import Foundation


public class HTTPValidationError: JSONEncodable {
    public var detail: [ValidationError]?

    public init() {}

    // MARK: JSONEncodable
    func encodeToJSON() -> AnyObject {
        var nillableDictionary = [String:AnyObject?]()
        nillableDictionary["detail"] = self.detail?.encodeToJSON()
        let dictionary: [String:AnyObject] = APIHelper.rejectNil(nillableDictionary) ?? [:]
        return dictionary
    }
}

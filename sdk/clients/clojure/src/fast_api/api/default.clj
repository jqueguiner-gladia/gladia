(ns fast-api.api.default
  (:require [fast-api.core :refer [call-api check-required-params with-collection-format *api-context*]]
            [clojure.spec.alpha :as s]
            [spec-tools.core :as st]
            [orchestra.core :refer [defn-spec]]
            [fast-api.specs.validation-error :refer :all]
            [fast-api.specs.http-validation-error :refer :all]
            )
  (:import (java.io File)))


(defn-spec read-users-image-image-uncolorization-users-get-with-http-info any?
  "Read Users"
  []
  (call-api "/image/image/uncolorization/users/" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json"]
             :auth-names    []}))

(defn-spec read-users-image-image-uncolorization-users-get any?
  "Read Users"
  []
  (let [res (:data (read-users-image-image-uncolorization-users-get-with-http-info))]
    (if (:decode-models *api-context*)
       (st/decode any? res st/string-transformer)
       res)))


(defn-spec root-get-with-http-info any?
  "Root"
  []
  (call-api "/" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json"]
             :auth-names    []}))

(defn-spec root-get any?
  "Root"
  []
  (let [res (:data (root-get-with-http-info))]
    (if (:decode-models *api-context*)
       (st/decode any? res st/string-transformer)
       res)))



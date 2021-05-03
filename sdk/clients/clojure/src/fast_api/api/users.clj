(ns fast-api.api.users
  (:require [fast-api.core :refer [call-api check-required-params with-collection-format *api-context*]]
            [clojure.spec.alpha :as s]
            [spec-tools.core :as st]
            [orchestra.core :refer [defn-spec]]
            [fast-api.specs.validation-error :refer :all]
            [fast-api.specs.http-validation-error :refer :all]
            )
  (:import (java.io File)))


(defn-spec read-user-me-image-image-uncolorization-users-me-get-with-http-info any?
  "Read User Me"
  []
  (call-api "/image/image/uncolorization/users/me" :get
            {:path-params   {}
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json"]
             :auth-names    []}))

(defn-spec read-user-me-image-image-uncolorization-users-me-get any?
  "Read User Me"
  []
  (let [res (:data (read-user-me-image-image-uncolorization-users-me-get-with-http-info))]
    (if (:decode-models *api-context*)
       (st/decode any? res st/string-transformer)
       res)))



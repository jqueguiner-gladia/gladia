(ns fast-api.api.us
  (:require [fast-api.core :refer [call-api check-required-params with-collection-format *api-context*]]
            [clojure.spec.alpha :as s]
            [spec-tools.core :as st]
            [orchestra.core :refer [defn-spec]]
            [fast-api.specs.validation-error :refer :all]
            [fast-api.specs.http-validation-error :refer :all]
            )
  (:import (java.io File)))


(defn-spec read-user-image-image-uncolorization-users-username-post-with-http-info any?
  "Read User"
  [username string?]
  (check-required-params username)
  (call-api "/image/image/uncolorization/users/{username}" :post
            {:path-params   {"username" username }
             :header-params {}
             :query-params  {}
             :form-params   {}
             :content-types []
             :accepts       ["application/json"]
             :auth-names    []}))

(defn-spec read-user-image-image-uncolorization-users-username-post any?
  "Read User"
  [username string?]
  (let [res (:data (read-user-image-image-uncolorization-users-username-post-with-http-info username))]
    (if (:decode-models *api-context*)
       (st/decode any? res st/string-transformer)
       res)))



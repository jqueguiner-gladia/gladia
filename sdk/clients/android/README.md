# openapi-android-client

## Requirements

Building the API client library requires [Maven](https://maven.apache.org/) to be installed.

## Installation

To install the API client library to your local Maven repository, simply execute:

```shell
mvn install
```

To deploy it to a remote Maven repository instead, configure the settings of the repository and execute:

```shell
mvn deploy
```

Refer to the [official documentation](https://maven.apache.org/plugins/maven-deploy-plugin/usage.html) for more information.

### Maven users

Add this dependency to your project's POM:

```xml
<dependency>
    <groupId>org.openapitools</groupId>
    <artifactId>openapi-android-client</artifactId>
    <version>1.0.0</version>
    <scope>compile</scope>
</dependency>
```

### Gradle users

Add this dependency to your project's build file:

```groovy
compile "org.openapitools:openapi-android-client:1.0.0"
```

### Others

At first generate the JAR by executing:

    mvn package

Then manually install the following JARs:

- target/openapi-android-client-1.0.0.jar
- target/lib/*.jar

## Getting Started

Please follow the [installation](#installation) instruction and execute the following Java code:

```java

import org.openapitools.client.api.DefaultApi;

public class DefaultApiExample {

    public static void main(String[] args) {
        DefaultApi apiInstance = new DefaultApi();
        try {
            Object result = apiInstance.readUsersImageImageUncolorizationUsersGet();
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling DefaultApi#readUsersImageImageUncolorizationUsersGet");
            e.printStackTrace();
        }
    }
}

```

## Documentation for API Endpoints

All URIs are relative to *http://localhost*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultApi* | [**readUsersImageImageUncolorizationUsersGet**](docs/DefaultApi.md#readUsersImageImageUncolorizationUsersGet) | **GET** /image/image/uncolorization/users/ | Read Users
*DefaultApi* | [**rootGet**](docs/DefaultApi.md#rootGet) | **GET** / | Root
*UsApi* | [**readUserImageImageUncolorizationUsersUsernamePost**](docs/UsApi.md#readUserImageImageUncolorizationUsersUsernamePost) | **POST** /image/image/uncolorization/users/{username} | Read User
*UsersApi* | [**readUserMeImageImageUncolorizationUsersMeGet**](docs/UsersApi.md#readUserMeImageImageUncolorizationUsersMeGet) | **GET** /image/image/uncolorization/users/me | Read User Me


## Documentation for Models

 - [HTTPValidationError](docs/HTTPValidationError.md)
 - [ValidationError](docs/ValidationError.md)


## Documentation for Authorization

All endpoints do not require authorization.
Authentication schemes defined for the API:

## Recommendation

It's recommended to create an instance of `ApiClient` per thread in a multithreaded environment to avoid any potential issues.

## Author




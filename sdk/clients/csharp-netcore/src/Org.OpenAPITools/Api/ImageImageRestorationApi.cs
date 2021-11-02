/* 
 * FastAPI
 *
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 0.1.0
 * 
 * Generated by: https://github.com/openapitools/openapi-generator.git
 */


using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Net;
using System.Net.Mime;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Org.OpenAPITools.Api
{

    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public interface IImageImageRestorationApiSync : IApiAccessor
    {
        #region Synchronous Operations
        /// <summary>
        /// Apply model for the restoration task for a given models
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="image"></param>
        /// <param name="model"> (optional, default to &quot;bringing-old-photos-back-to-life&quot;)</param>
        /// <returns>Object</returns>
        Object ApplyImageImageRestorationPost (System.IO.Stream image, string model = null);

        /// <summary>
        /// Apply model for the restoration task for a given models
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="image"></param>
        /// <param name="model"> (optional, default to &quot;bringing-old-photos-back-to-life&quot;)</param>
        /// <returns>ApiResponse of Object</returns>
        ApiResponse<Object> ApplyImageImageRestorationPostWithHttpInfo (System.IO.Stream image, string model = null);
        /// <summary>
        /// Get list of models available for restoration
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Object</returns>
        Object GetVersionsImageImageRestorationGet ();

        /// <summary>
        /// Get list of models available for restoration
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>ApiResponse of Object</returns>
        ApiResponse<Object> GetVersionsImageImageRestorationGetWithHttpInfo ();
        #endregion Synchronous Operations
    }

    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public interface IImageImageRestorationApiAsync : IApiAccessor
    {
        #region Asynchronous Operations
        /// <summary>
        /// Apply model for the restoration task for a given models
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="image"></param>
        /// <param name="model"> (optional, default to &quot;bringing-old-photos-back-to-life&quot;)</param>
        /// <returns>Task of Object</returns>
        System.Threading.Tasks.Task<Object> ApplyImageImageRestorationPostAsync (System.IO.Stream image, string model = null);

        /// <summary>
        /// Apply model for the restoration task for a given models
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="image"></param>
        /// <param name="model"> (optional, default to &quot;bringing-old-photos-back-to-life&quot;)</param>
        /// <returns>Task of ApiResponse (Object)</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> ApplyImageImageRestorationPostAsyncWithHttpInfo (System.IO.Stream image, string model = null);
        /// <summary>
        /// Get list of models available for restoration
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Task of Object</returns>
        System.Threading.Tasks.Task<Object> GetVersionsImageImageRestorationGetAsync ();

        /// <summary>
        /// Get list of models available for restoration
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Task of ApiResponse (Object)</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> GetVersionsImageImageRestorationGetAsyncWithHttpInfo ();
        #endregion Asynchronous Operations
    }

    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public interface IImageImageRestorationApi : IImageImageRestorationApiSync, IImageImageRestorationApiAsync
    {

    }

    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public partial class ImageImageRestorationApi : IImageImageRestorationApi
    {
        private Org.OpenAPITools.Client.ExceptionFactory _exceptionFactory = (name, response) => null;

        /// <summary>
        /// Initializes a new instance of the <see cref="ImageImageRestorationApi"/> class.
        /// </summary>
        /// <returns></returns>
        public ImageImageRestorationApi() : this((string) null)
        {
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ImageImageRestorationApi"/> class.
        /// </summary>
        /// <returns></returns>
        public ImageImageRestorationApi(String basePath)
        {
            this.Configuration = Org.OpenAPITools.Client.Configuration.MergeConfigurations(
                Org.OpenAPITools.Client.GlobalConfiguration.Instance,
                new Org.OpenAPITools.Client.Configuration { BasePath = basePath }
            );
            this.Client = new Org.OpenAPITools.Client.ApiClient(this.Configuration.BasePath);
            this.AsynchronousClient = new Org.OpenAPITools.Client.ApiClient(this.Configuration.BasePath);
            this.ExceptionFactory = Org.OpenAPITools.Client.Configuration.DefaultExceptionFactory;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ImageImageRestorationApi"/> class
        /// using Configuration object
        /// </summary>
        /// <param name="configuration">An instance of Configuration</param>
        /// <returns></returns>
        public ImageImageRestorationApi(Org.OpenAPITools.Client.Configuration configuration)
        {
            if (configuration == null) throw new ArgumentNullException("configuration");

            this.Configuration = Org.OpenAPITools.Client.Configuration.MergeConfigurations(
                Org.OpenAPITools.Client.GlobalConfiguration.Instance,
                configuration
            );
            this.Client = new Org.OpenAPITools.Client.ApiClient(this.Configuration.BasePath);
            this.AsynchronousClient = new Org.OpenAPITools.Client.ApiClient(this.Configuration.BasePath);
            ExceptionFactory = Org.OpenAPITools.Client.Configuration.DefaultExceptionFactory;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="PetApi"/> class
        /// using a Configuration object and client instance.
        /// </summary>
        /// <param name="client">The client interface for synchronous API access.</param>
        /// <param name="asyncClient">The client interface for asynchronous API access.</param>
        /// <param name="configuration">The configuration object.</param>
        public ImageImageRestorationApi(Org.OpenAPITools.Client.ISynchronousClient client,Org.OpenAPITools.Client.IAsynchronousClient asyncClient, Org.OpenAPITools.Client.IReadableConfiguration configuration)
        {
            if(client == null) throw new ArgumentNullException("client");
            if(asyncClient == null) throw new ArgumentNullException("asyncClient");
            if(configuration == null) throw new ArgumentNullException("configuration");

            this.Client = client;
            this.AsynchronousClient = asyncClient;
            this.Configuration = configuration;
            this.ExceptionFactory = Org.OpenAPITools.Client.Configuration.DefaultExceptionFactory;
        }

        /// <summary>
        /// The client for accessing this underlying API asynchronously.
        /// </summary>
        public Org.OpenAPITools.Client.IAsynchronousClient AsynchronousClient { get; set; }

        /// <summary>
        /// The client for accessing this underlying API synchronously.
        /// </summary>
        public Org.OpenAPITools.Client.ISynchronousClient Client { get; set; }

        /// <summary>
        /// Gets the base path of the API client.
        /// </summary>
        /// <value>The base path</value>
        public String GetBasePath()
        {
            return this.Configuration.BasePath;
        }

        /// <summary>
        /// Gets or sets the configuration object
        /// </summary>
        /// <value>An instance of the Configuration</value>
        public Org.OpenAPITools.Client.IReadableConfiguration Configuration {get; set;}

        /// <summary>
        /// Provides a factory method hook for the creation of exceptions.
        /// </summary>
        public Org.OpenAPITools.Client.ExceptionFactory ExceptionFactory
        {
            get
            {
                if (_exceptionFactory != null && _exceptionFactory.GetInvocationList().Length > 1)
                {
                    throw new InvalidOperationException("Multicast delegate for ExceptionFactory is unsupported.");
                }
                return _exceptionFactory;
            }
            set { _exceptionFactory = value; }
        }

        /// <summary>
        /// Apply model for the restoration task for a given models 
        /// </summary>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="image"></param>
        /// <param name="model"> (optional, default to &quot;bringing-old-photos-back-to-life&quot;)</param>
        /// <returns>Object</returns>
        public Object ApplyImageImageRestorationPost (System.IO.Stream image, string model = null)
        {
             Org.OpenAPITools.Client.ApiResponse<Object> localVarResponse = ApplyImageImageRestorationPostWithHttpInfo(image, model);
             return localVarResponse.Data;
        }

        /// <summary>
        /// Apply model for the restoration task for a given models 
        /// </summary>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="image"></param>
        /// <param name="model"> (optional, default to &quot;bringing-old-photos-back-to-life&quot;)</param>
        /// <returns>ApiResponse of Object</returns>
        public Org.OpenAPITools.Client.ApiResponse< Object > ApplyImageImageRestorationPostWithHttpInfo (System.IO.Stream image, string model = null)
        {
            // verify the required parameter 'image' is set
            if (image == null)
                throw new Org.OpenAPITools.Client.ApiException(400, "Missing required parameter 'image' when calling ImageImageRestorationApi->ApplyImageImageRestorationPost");

            Org.OpenAPITools.Client.RequestOptions requestOptions = new Org.OpenAPITools.Client.RequestOptions();

            String[] @contentTypes = new String[] {
                "multipart/form-data"
            };

            // to determine the Accept header
            String[] @accepts = new String[] {
                "application/json"
            };

            var localVarContentType = Org.OpenAPITools.Client.ClientUtils.SelectHeaderContentType(@contentTypes);
            if (localVarContentType != null) requestOptions.HeaderParameters.Add("Content-Type", localVarContentType);

            var localVarAccept = Org.OpenAPITools.Client.ClientUtils.SelectHeaderAccept(@accepts);
            if (localVarAccept != null) requestOptions.HeaderParameters.Add("Accept", localVarAccept);

            if (model != null)
            {
                foreach (var kvp in Org.OpenAPITools.Client.ClientUtils.ParameterToMultiMap("", "model", model))
                {
                    foreach (var value in kvp.Value)
                    {
                        requestOptions.QueryParameters.Add(kvp.Key, value);
                    }
                }
            }
            if (image != null)
            {
                requestOptions.FileParameters.Add("image", image);
            }


            // make the HTTP request

            var response = this.Client.Post< Object >("/image/image/restoration/", requestOptions, this.Configuration);

            if (this.ExceptionFactory != null)
            {
                Exception exception = this.ExceptionFactory("ApplyImageImageRestorationPost", response);
                if (exception != null) throw exception;
            }

            return response;
        }

        /// <summary>
        /// Apply model for the restoration task for a given models 
        /// </summary>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="image"></param>
        /// <param name="model"> (optional, default to &quot;bringing-old-photos-back-to-life&quot;)</param>
        /// <returns>Task of Object</returns>
        public async System.Threading.Tasks.Task<Object> ApplyImageImageRestorationPostAsync (System.IO.Stream image, string model = null)
        {
             Org.OpenAPITools.Client.ApiResponse<Object> localVarResponse = await ApplyImageImageRestorationPostAsyncWithHttpInfo(image, model);
             return localVarResponse.Data;

        }

        /// <summary>
        /// Apply model for the restoration task for a given models 
        /// </summary>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="image"></param>
        /// <param name="model"> (optional, default to &quot;bringing-old-photos-back-to-life&quot;)</param>
        /// <returns>Task of ApiResponse (Object)</returns>
        public async System.Threading.Tasks.Task<Org.OpenAPITools.Client.ApiResponse<Object>> ApplyImageImageRestorationPostAsyncWithHttpInfo (System.IO.Stream image, string model = null)
        {
            // verify the required parameter 'image' is set
            if (image == null)
                throw new Org.OpenAPITools.Client.ApiException(400, "Missing required parameter 'image' when calling ImageImageRestorationApi->ApplyImageImageRestorationPost");


            Org.OpenAPITools.Client.RequestOptions requestOptions = new Org.OpenAPITools.Client.RequestOptions();

            String[] @contentTypes = new String[] {
                "multipart/form-data"
            };

            // to determine the Accept header
            String[] @accepts = new String[] {
                "application/json"
            };
            
            foreach (var contentType in @contentTypes)
                requestOptions.HeaderParameters.Add("Content-Type", contentType);
            
            foreach (var accept in @accepts)
                requestOptions.HeaderParameters.Add("Accept", accept);
            
            if (model != null)
            {
                foreach (var kvp in Org.OpenAPITools.Client.ClientUtils.ParameterToMultiMap("", "model", model))
                {
                    foreach (var value in kvp.Value)
                    {
                        requestOptions.QueryParameters.Add(kvp.Key, value);
                    }
                }
            }
            if (image != null)
            {
                requestOptions.FileParameters.Add("image", image);
            }


            // make the HTTP request

            var response = await this.AsynchronousClient.PostAsync<Object>("/image/image/restoration/", requestOptions, this.Configuration);

            if (this.ExceptionFactory != null)
            {
                Exception exception = this.ExceptionFactory("ApplyImageImageRestorationPost", response);
                if (exception != null) throw exception;
            }

            return response;
        }

        /// <summary>
        /// Get list of models available for restoration 
        /// </summary>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Object</returns>
        public Object GetVersionsImageImageRestorationGet ()
        {
             Org.OpenAPITools.Client.ApiResponse<Object> localVarResponse = GetVersionsImageImageRestorationGetWithHttpInfo();
             return localVarResponse.Data;
        }

        /// <summary>
        /// Get list of models available for restoration 
        /// </summary>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>ApiResponse of Object</returns>
        public Org.OpenAPITools.Client.ApiResponse< Object > GetVersionsImageImageRestorationGetWithHttpInfo ()
        {
            Org.OpenAPITools.Client.RequestOptions requestOptions = new Org.OpenAPITools.Client.RequestOptions();

            String[] @contentTypes = new String[] {
            };

            // to determine the Accept header
            String[] @accepts = new String[] {
                "application/json"
            };

            var localVarContentType = Org.OpenAPITools.Client.ClientUtils.SelectHeaderContentType(@contentTypes);
            if (localVarContentType != null) requestOptions.HeaderParameters.Add("Content-Type", localVarContentType);

            var localVarAccept = Org.OpenAPITools.Client.ClientUtils.SelectHeaderAccept(@accepts);
            if (localVarAccept != null) requestOptions.HeaderParameters.Add("Accept", localVarAccept);



            // make the HTTP request

            var response = this.Client.Get< Object >("/image/image/restoration/", requestOptions, this.Configuration);

            if (this.ExceptionFactory != null)
            {
                Exception exception = this.ExceptionFactory("GetVersionsImageImageRestorationGet", response);
                if (exception != null) throw exception;
            }

            return response;
        }

        /// <summary>
        /// Get list of models available for restoration 
        /// </summary>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Task of Object</returns>
        public async System.Threading.Tasks.Task<Object> GetVersionsImageImageRestorationGetAsync ()
        {
             Org.OpenAPITools.Client.ApiResponse<Object> localVarResponse = await GetVersionsImageImageRestorationGetAsyncWithHttpInfo();
             return localVarResponse.Data;

        }

        /// <summary>
        /// Get list of models available for restoration 
        /// </summary>
        /// <exception cref="Org.OpenAPITools.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Task of ApiResponse (Object)</returns>
        public async System.Threading.Tasks.Task<Org.OpenAPITools.Client.ApiResponse<Object>> GetVersionsImageImageRestorationGetAsyncWithHttpInfo ()
        {

            Org.OpenAPITools.Client.RequestOptions requestOptions = new Org.OpenAPITools.Client.RequestOptions();

            String[] @contentTypes = new String[] {
            };

            // to determine the Accept header
            String[] @accepts = new String[] {
                "application/json"
            };
            
            foreach (var contentType in @contentTypes)
                requestOptions.HeaderParameters.Add("Content-Type", contentType);
            
            foreach (var accept in @accepts)
                requestOptions.HeaderParameters.Add("Accept", accept);
            


            // make the HTTP request

            var response = await this.AsynchronousClient.GetAsync<Object>("/image/image/restoration/", requestOptions, this.Configuration);

            if (this.ExceptionFactory != null)
            {
                Exception exception = this.ExceptionFactory("GetVersionsImageImageRestorationGet", response);
                if (exception != null) throw exception;
            }

            return response;
        }

    }
}

package org.openapitools.api;

import java.io.File;
import org.openapitools.model.HTTPValidationError;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ApiResponse;
import io.swagger.jaxrs.PATCH;

/**
 * FastAPI
 *
 * <p>No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 */
@Path("/")
@Api(value = "/", description = "")
public interface ImageImageUncolorizationApi  {

    /**
     * Apply model for the uncolorization task for a given models
     *
     */
    @POST
    @Path("/image/image/uncolorization/")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @ApiOperation(value = "Apply model for the uncolorization task for a given models", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Successful Response", response = Object.class),
        @ApiResponse(code = 422, message = "Validation Error", response = HTTPValidationError.class) })
    public Object applyImageImageUncolorizationPost( @Multipart(value = "image" ) Attachment imageDetail, @QueryParam("model") @DefaultValue("v1")String model);

    /**
     * Get list of models available for uncolorization
     *
     */
    @GET
    @Path("/image/image/uncolorization/")
    @Produces({ "application/json" })
    @ApiOperation(value = "Get list of models available for uncolorization", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Successful Response", response = Object.class) })
    public Object getVersionsImageImageUncolorizationGet();
}


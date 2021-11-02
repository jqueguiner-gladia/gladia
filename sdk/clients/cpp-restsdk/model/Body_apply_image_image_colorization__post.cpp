/**
 * FastAPI
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 0.1.0
 *
 * NOTE: This class is auto generated by OpenAPI-Generator 4.0.0.
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */



#include "Body_apply_image_image_colorization__post.h"

namespace org {
namespace openapitools {
namespace client {
namespace model {




Body_apply_image_image_colorization__post::Body_apply_image_image_colorization__post()
{
}

Body_apply_image_image_colorization__post::~Body_apply_image_image_colorization__post()
{
}

void Body_apply_image_image_colorization__post::validate()
{
    // TODO: implement validation
}

web::json::value Body_apply_image_image_colorization__post::toJson() const
{
    web::json::value val = web::json::value::object();

    val[utility::conversions::to_string_t("image")] = ModelBase::toJson(m_Image);

    return val;
}

void Body_apply_image_image_colorization__post::fromJson(const web::json::value& val)
{
    setImage(ModelBase::fileFromJson(val.at(utility::conversions::to_string_t("image"))));
}

void Body_apply_image_image_colorization__post::toMultipart(std::shared_ptr<MultipartFormData> multipart, const utility::string_t& prefix) const
{
    utility::string_t namePrefix = prefix;
    if(namePrefix.size() > 0 && namePrefix.substr(namePrefix.size() - 1) != utility::conversions::to_string_t("."))
    {
        namePrefix += utility::conversions::to_string_t(".");
    }

    multipart->add(ModelBase::toHttpContent(namePrefix + utility::conversions::to_string_t("image"), m_Image));
}

void Body_apply_image_image_colorization__post::fromMultiPart(std::shared_ptr<MultipartFormData> multipart, const utility::string_t& prefix)
{
    utility::string_t namePrefix = prefix;
    if(namePrefix.size() > 0 && namePrefix.substr(namePrefix.size() - 1) != utility::conversions::to_string_t("."))
    {
        namePrefix += utility::conversions::to_string_t(".");
    }

    setImage(multipart->getContent(utility::conversions::to_string_t("image")));
}

HttpContent Body_apply_image_image_colorization__post::getImage() const
{
    return m_Image;
}

void Body_apply_image_image_colorization__post::setImage(const HttpContent& value)
{
    m_Image = value;
    
}

}
}
}
}



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
using System.Linq;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Runtime.Serialization;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using System.ComponentModel.DataAnnotations;
using OpenAPIDateConverter = Org.OpenAPITools.Client.OpenAPIDateConverter;

namespace Org.OpenAPITools.Model
{
    /// <summary>
    /// BodyApplyImageImageFaceBluringPost
    /// </summary>
    [DataContract]
    public partial class BodyApplyImageImageFaceBluringPost :  IEquatable<BodyApplyImageImageFaceBluringPost>, IValidatableObject
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="BodyApplyImageImageFaceBluringPost" /> class.
        /// </summary>
        [JsonConstructorAttribute]
        protected BodyApplyImageImageFaceBluringPost() { }
        /// <summary>
        /// Initializes a new instance of the <see cref="BodyApplyImageImageFaceBluringPost" /> class.
        /// </summary>
        /// <param name="image">image (required).</param>
        public BodyApplyImageImageFaceBluringPost(System.IO.Stream image = default(System.IO.Stream))
        {
            // to ensure "image" is required (not null)
            if (image == null)
            {
                throw new InvalidDataException("image is a required property for BodyApplyImageImageFaceBluringPost and cannot be null");
            }
            else
            {
                this.Image = image;
            }

        }
        
        /// <summary>
        /// Gets or Sets Image
        /// </summary>
        [DataMember(Name="image", EmitDefaultValue=false)]
        public System.IO.Stream Image { get; set; }

        /// <summary>
        /// Returns the string presentation of the object
        /// </summary>
        /// <returns>String presentation of the object</returns>
        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("class BodyApplyImageImageFaceBluringPost {\n");
            sb.Append("  Image: ").Append(Image).Append("\n");
            sb.Append("}\n");
            return sb.ToString();
        }
  
        /// <summary>
        /// Returns the JSON string presentation of the object
        /// </summary>
        /// <returns>JSON string presentation of the object</returns>
        public virtual string ToJson()
        {
            return JsonConvert.SerializeObject(this, Formatting.Indented);
        }

        /// <summary>
        /// Returns true if objects are equal
        /// </summary>
        /// <param name="input">Object to be compared</param>
        /// <returns>Boolean</returns>
        public override bool Equals(object input)
        {
            return this.Equals(input as BodyApplyImageImageFaceBluringPost);
        }

        /// <summary>
        /// Returns true if BodyApplyImageImageFaceBluringPost instances are equal
        /// </summary>
        /// <param name="input">Instance of BodyApplyImageImageFaceBluringPost to be compared</param>
        /// <returns>Boolean</returns>
        public bool Equals(BodyApplyImageImageFaceBluringPost input)
        {
            if (input == null)
                return false;

            return 
                (
                    this.Image == input.Image ||
                    (this.Image != null &&
                    this.Image.Equals(input.Image))
                );
        }

        /// <summary>
        /// Gets the hash code
        /// </summary>
        /// <returns>Hash code</returns>
        public override int GetHashCode()
        {
            unchecked // Overflow is fine, just wrap
            {
                int hashCode = 41;
                if (this.Image != null)
                    hashCode = hashCode * 59 + this.Image.GetHashCode();
                return hashCode;
            }
        }

        /// <summary>
        /// To validate all properties of the instance
        /// </summary>
        /// <param name="validationContext">Validation context</param>
        /// <returns>Validation Result</returns>
        IEnumerable<System.ComponentModel.DataAnnotations.ValidationResult> IValidatableObject.Validate(ValidationContext validationContext)
        {
            yield break;
        }
    }

}

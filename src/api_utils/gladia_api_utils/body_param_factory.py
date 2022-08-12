from typing import Any

from pydantic import BaseModel, create_model


def create_schema_for_body_param(
    schema_name: str, param_name: str, param_type: Any, example: Any
) -> BaseModel:
    class Config:
        schema_extra = {
            "example": example,
        }

    data = {param_name: (param_type, ...)}

    return create_model(schema_name, **data, __config__=Config)

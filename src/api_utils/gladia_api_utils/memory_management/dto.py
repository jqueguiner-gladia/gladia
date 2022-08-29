from enum import Enum


class ModelType(str, Enum):
    TRITON = 'triton'
    FASTAPI = 'fastapi'
    LOCAL = 'local'


class ModelStatus(str, Enum):
    READY = 'ready'
    LOADED = 'loaded'
    NOT_READY = 'not-ready'

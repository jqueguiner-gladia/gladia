import logging
import os
from logging import StreamHandler
from logging.handlers import RotatingFileHandler

from .get_activated_task_path import get_activated_task_path

logging_format = os.getenv(
    "API_UTILS_LOGGING_FORMAT", "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)

logging_level = {
    None: logging.NOTSET,
    "": logging.NOTSET,
    "none": logging.NOTSET,
    "debug": logging.DEBUG,
    "info": logging.INFO,
    "warning": logging.WARNING,
    "error": logging.ERROR,
    "critical": logging.CRITICAL,
}.get(os.getenv("API_UTILS_LOGGING_LEVEL", "info"), logging.INFO)

logging.basicConfig(
    level=logging_level,
    format=logging_format,
)

logger = logging.getLogger(__name__)

log_path = os.getenv("API_UTILS_LOGGING_PATH", "./.api_utils.logs")
rotating_file_handler = RotatingFileHandler(
    log_path,
    maxBytes=2000,
    backupCount=10,
)
rotating_file_handler.setFormatter(logging.Formatter(logging_format))

stream_handler = StreamHandler()
stream_handler.setFormatter(logging.Formatter(logging_format))
stream_handler.setLevel(logging_level)

logger.addHandler(rotating_file_handler)
logger.addHandler(stream_handler)

__all__ = ["get_activated_task_path"]

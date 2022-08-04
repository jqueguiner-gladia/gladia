import logging
import os
from logging.handlers import RotatingFileHandler

from .get_activated_task_path import get_activated_task_path

logging_format = os.getenv(
    "API_UTILS_LOGGING_FORMAT", "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)

logging.basicConfig(
    level={
        None: logging.NOTSET,
        "": logging.NOTSET,
        "none": logging.NOTSET,
        "debug": logging.DEBUG,
        "info": logging.INFO,
        "warning": logging.WARNING,
        "error": logging.ERROR,
        "critical": logging.CRITICAL,
    }.get(os.getenv("API_UTILS_LOGGING_LEVEL", "info"), logging.INFO),
    format=logging_format,
)

logger = logging.getLogger(__name__)

handler = RotatingFileHandler(
    os.getenv("API_UTILS_LOGGING_PATH", "./.api_utils.logs"),
    maxBytes=2000,
    backupCount=10,
)
handler.setFormatter(logging.Formatter(logging_format))

logger.addHandler(handler)

__all__ = ["get_activated_task_path"]

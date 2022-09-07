import io
import os
import random
import string
import sys
import tempfile
from logging import getLogger
from pathlib import Path
from typing import Any, Tuple

import gdown
import magic
import requests
from PIL import Image
from xtract import XZ, BZip2, GZip, Rar, Tar, Zip, xtract
from xtract.utils import get_file_type
import re

PATTERN = re.compile(r'((\w:)|(\.))((/(?!/)(?!/)|\\{2})[^\n?"|></\\:*]+)+')


logger = getLogger(__name__)


def is_binary_file(file_path: str) -> bool:
    """
    Check if a file is binary or not.

    Args:
        file_path (str): The path to the file to check.

    Returns:
        bool: True if the file is binary, False otherwise.
    """

    textchars = bytearray({7, 8, 9, 10, 12, 13, 27} | set(range(0x20, 0x100)) - {0x7F})
    is_binary_string = lambda bytes: bool(bytes.translate(None, textchars))

    return is_binary_string(open(file_path, "rb").read(1024))


def is_valid_path(string: str) -> bool:
    """
    Check if a string is a valid path.

    Args:
        string (str): The string to check.

    Returns:
        bool: True if the string is a valid path, False otherwise.
    """

    if string and isinstance(string, str) and PATTERN.match(string):
        return True
    else:
        return False


def get_tmp_filename() -> str:
    """
    Get a random temporary fullpath.

    Returns:
        str: The random filepath.
    """
    return str(
        os.path.join(
            tempfile._get_default_tempdir(), next(tempfile._get_candidate_names())
        )
    )


def write_tmp_file(content: Any) -> str:
    """
    Write content to a temporary file.

    Args:
        content (Any): The content to write.

    Returns:
        str: The path to the temporary file.
    """

    tmp = tempfile.NamedTemporaryFile(delete=False)
    tmp.write(content)
    tmp.close()

    return tmp.name


def input_to_files(func):
    """
    Decorator that converts input to a list of files.

    Args:
        func (function): The function to decorate.

    Returns:
        function: The decorated function.
    """

    def inner(*args, **kwargs) -> Any:
        """
        Wrapper for the decorated function.

        Args:
            *args: The arguments to the function.
            **kwargs: The keyword arguments to the function.

        Returns:
            Any: The return value of the function.
        """
        tmp_files = list()

        for keyword, arg in kwargs.items():
            if isinstance(arg, (bytes, bytearray)):
                tmp_file = write_tmp_file(arg)
                tmp_files.append(tmp_file)
                kwargs[keyword] = tmp_file

        result = func(*args, **kwargs)

        try:
            delete_all_files(tmp_files)
        except Exception as e:
            logger.error(f"Couldn't delete tmp files: {e}")

        return result

    return inner


def download_file(
    url: str,
    file_full_path: str,
    force_create_dir: bool = True,
    force_redownload: bool = False,
) -> Path:
    """
    Download a file from a url.

    Args:
        url (str): The url to download the file from.
        file_full_path (str): The path to the file to download.
        force_create_dir (bool): Create the directory if it doesn't exist. (default: True)
        force_redownload (bool): Redownload the file if it already exists. (default: False)

    Returns:
        Path: The path to the downloaded file.
    """

    is_gdrivefile = "https://drive.google.com/" in url

    file_full_path = Path(file_full_path)

    if not os.path.exists(file_full_path.parent):
        if force_create_dir:
            create_directory(file_full_path.parent)
        else:
            raise Exception(
                "Parent directory doesn't exists : change the path or use force_create_dir = True"
            )

    if not file_full_path.exists():
        if is_gdrivefile:
            gdown.download(url, file_full_path, quiet=False)
        else:
            write_url_content_to_file(file_full_path, url)

    elif force_redownload:
        if is_gdrivefile:
            gdown.download(url, file_full_path, quiet=False)
        else:
            write_url_content_to_file(file_full_path, url)

    return file_full_path


def write_url_content_to_file(file_full_path: Path, url: str) -> bool:
    """
    Write the content of a url to a file.

    Args:
        file_full_path (Path): The path to the file to write.
        url (str): The url to download the content from.

    Returns:
        bool: True if the file was written, False otherwise.
    """

    data = requests.get(url).content

    logger.debug(f"writing {url} to {file_full_path}")

    return write_to_file(file_full_path, data)


def write_to_file(file_full_path: str, data: Any, overwrite: bool = False) -> bool:
    """
    Write data to a file.

    Args:
        file_full_path (str): The path to the file to write.
        data (Any): The data to write.
        overwrite (bool): Overwrite the file if it already exists. (default: False)

    Returns:
        bool: True if the file was written, False otherwise.
    """

    if isinstance(file_full_path, str):
        file_full_path = Path(file_full_path)

    os.makedirs(os.path.dirname(file_full_path), exist_ok=True)

    if not os.path.exists(file_full_path) or overwrite:
        if isinstance(data, io.BytesIO):
            with open(file_full_path, "wb") as handler:
                data = data.read()
            handler.write(data)
        elif isinstance(data, Image.Image):
            data.save(file_full_path)
        else:
            with open(file_full_path, "wb") as handler:
                handler.write(data)


def delete_file(filepath: str) -> bool:
    """
    Delete a file.

    Args:
        filepath (str): The path to the file to delete.

    Returns:
        bool: True if the file was deleted, False otherwise.
    """

    if os.path.exists(filepath):
        return os.remove(filepath)
    else:
        return False


def delete_all_files(files: list) -> list:
    """
    Delete all files in a given list

    Args:
        files (list): The list of files to delete.

    Returns:
        list: The list of files that were deleted.
    """

    out = dict()
    for file in files:
        out[file] = delete_file(file)
    return out


def delete_directory(dirpath: str) -> bool:
    """
    Delete a file.

    Args:
        dirpath (str): The path to the directory to delete.

    Returns:
        bool: True if the directory was deleted, False otherwise.
    """

    delete_file(dirpath)


def create_directory(path: str) -> bool:
    """
    Create a directory.

    Args:
        path (str): The path to the directory to create.

    Returns:
        bool: True if the directory was created, False otherwise.
    """

    if len(os.path.dirname(path)) > 0:
        return os.makedirs(os.path.dirname(path), exist_ok=True)
    else:
        return False


def uncompress(
    path: str, destination: str = None, delete_after_uncompress: bool = True
) -> str:
    """
    Uncompress a file.

    Args:
        path (str): The path to the file to uncompress.
        destination (str): The path to the destination directory. If None, the destination is the same as the path. (default: None)
        delete_after_uncompress (bool): Delete the file after uncompress. (default: True)

    Returns:
        str: The path to the uncompressed file.

    Raises:
        Exception: If the file is not a supported compression format.
    """

    try:
        logger.debug(f"Extracting archive from {path} to {destination}")
        output = xtract(path, destination=destination, overwrite=True, all=False)

        if delete_after_uncompress:
            logger.debug(f"Deleting archive {path}")
            delete_file(path)

        logger.debug(f"Extraction done to {output}")

        return output

    except:
        raise Exception(f"Error while uncompressing {path}")


def compress_directory(
    path: str,
    compression_format: str = "gzip",
    destination: str = None,
    delete_after_compress: bool = False,
) -> str:
    """
    Compress a directory and returns the path of the compressed file.

    Args:
        path (str): path to the directory to compress
        compression_format (str): format of the compression (rar, tar, zip, bz2, gz, xz) (default: gzip)
        destination (str): path to the destination of the compressed file. if None, the compressed file is in the same directory as the directory to compress (default: None)
        delete_after_compress (bool): delete the directory after compression (default: False)

    Returns:
        str: path to the compressed file
    """
    output = ""

    if destination:
        # used for relative paths
        namespace = sys._getframe(1).f_globals
        cwd = os.getcwd()
        rel_path = namespace["__file__"]
        root_path = os.path.dirname(os.path.join(cwd, rel_path))

        if not os.path.isabs(destination):
            destination = os.path.join(root_path, destination)

    if compression_format == "rar":
        output = Rar(path, destination=destination)

    if compression_format == "tar":
        output = Tar(path, destination=destination)

    if compression_format == "zip":
        output = Zip(path, destination=destination)

    if compression_format == "bz2":
        output = BZip2(path, destination=destination)

    if compression_format == "gz":
        output = GZip(path, destination=destination)

    if compression_format == "xz":
        output = XZ(path, destination=destination)

    if delete_after_compress:
        logger.debug(f"Deleting directory {path}")
        delete_directory(path)

    return output


def is_uncompressable(file_path: str) -> bool:
    """
    Returns True if the file is an uncompressable file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is an uncompressable file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    # exclude archive files that are not supported
    if file_path.endswith(
        (".ckpt", ".pt", ".pth", ".pkl", ".pth", ".pkl", ".ckpt.meta")
    ):
        return False
    else:
        return is_archive(file_path)


def is_archive(file_path: str) -> bool:
    """
    Returns True if the file is an archive file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is an archive file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    return get_file_category(file_path) == "archive"


def is_image(file_path: str) -> bool:
    """
    Returns True if the file is an image file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is an image file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    file_mime_type = get_file_category(file_path)

    return file_mime_type == "image"


def is_audio(file_path: str) -> bool:
    """
    Returns True if the file is an audio file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is an audio file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    file_mime_type = get_file_category(file_path)

    return file_mime_type == "audio"


def is_video(file_path: str) -> bool:
    """
    Returns True if the file is a video file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is a video file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    file_mime_type = get_file_category(file_path)

    return file_mime_type == "video"


def is_structured_data(file_path: str) -> bool:
    """
    Returns True if the file is a structured data file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is a structured data file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    file_mime_type = get_file_category(file_path)

    return file_mime_type == "structured_data"


def is_word(file_path: str) -> bool:
    """
    Returns True if the file is a word file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is a word file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    file_mime_type = get_file_category(file_path)

    return file_mime_type == "word"


def is_pdf(file_path: str) -> bool:
    """
    Returns True if the file is a pdf file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is a pdf file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    file_mime_type = get_file_category(file_path)

    return file_mime_type == "pdf"


def is_web_content(file_path: str) -> bool:
    """
    Returns True if the file is a web content file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        bool: True if the file is a web content file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    file_mime_type = get_file_category(file_path)

    return file_mime_type == "web_content"


def get_buffer_category(buffer: bytes) -> str:
    """
    Returns the category of the buffer.

    Args:
        buffer (bytes): The buffer to analyze.

    Returns:
        str: The category of the buffer.
    """

    return get_mime_category(get_buffer_type(buffer))


def get_file_category(file_path: str) -> str:
    """
    Returns the category of the file.

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        str: The category of the file.
    """

    output = ""
    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    file_mime_type = get_file_type(file_path)

    return get_mime_category(file_mime_type)


def get_mime_category(mime_type: str) -> str:
    """
    Returns the category of the mime type.

    Args:
        mime_type (str): The mime type to analyze.

    Returns:
        str: The category of the mime type.
    """

    if mime_type in [
        "audio/aac",
        "audio/midi",
        "audio/ogg",
        "audio/x-wav",
        "audio/webm",
        "audio/3gpp",
        "audio/3gpp2",
    ]:
        output = "audio"

    elif mime_type in ["application/octet-stream"]:
        output = "binary"

    elif mime_type in [
        "video/x-msvideo",
        "video/mpeg",
        "video/ogg",
        "video/webm",
        "video/3gpp",
        "video/3gpp2",
    ]:
        output = "video"

    elif mime_type in [
        "image/bmp",
        "image/gif",
        "image/x-icon",
        "image/jpeg",
        "image/png",
        "image/svg+xml",
        "image/tiff",
        "image/webp",
    ]:
        output = "image"

    elif mime_type in ["font/otf", "font/ttf", "font/woff", "font/woff2"]:
        output = "font"

    elif mime_type in ["application/vnd.amazon.ebook", "application/epub+zip"]:
        output = "ebook"

    elif mime_type in [
        "application/x-bzip",
        "application/x-bzip",
        "application/x-bzip2",
        "application/java-archive",
        "application/x-rar-compressed",
        "application/x-tar",
        "application/zip",
        "application/x-7z-compressed",
    ]:
        output = "archive"

    elif mime_type in [
        "application/x-csh",
        "application/ogg",
        "application/x-sh",
        "application/x-shockwave-flash",
    ]:
        output = "executable"

    elif mime_type in [
        "text/css",
        "text/html",
        "application/javascript",
        "application/xhtml+xml",
        "application/vnd.mozilla.xul+xml",
    ]:
        output = "web_content"

    elif mime_type in [
        "text/csv",
        "application/vnd.oasis.opendocument.spreadsheet",
        "application/vnd.ms-excel",
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    ]:
        output = "flat_structured_data"

    elif mime_type in ["application/json", "application/xml"]:
        output = "multidimensional_structured_data"

    elif mime_type in [
        "application/msword",
        "application/x-abiword",
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "application/vnd.oasis.opendocument.text",
        "application/rtf",
    ]:
        output = "word"

    elif mime_type in [
        "application/vnd.ms-powerpoint",
        "application/vnd.oasis.opendocument.presentation",
        "application/vnd.openxmlformats-officedocument.presentationml.presentation",
    ]:
        output = "presentation"

    elif mime_type in ["application/pdf"]:
        output = "pdf"

    elif mime_type in ["application/vnd.visio"]:
        output = "diagram"

    elif mime_type in ["text/plain"]:
        output = "text"

    else:
        output = "other"

    return output


def get_file_type(file_path: str) -> str:
    """
    Return the file mime type with mime type
    see full list here : https://developer.mozilla.org/fr/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types

    Args:
        file_path (str): The path to the file to analyze.

    Returns:
        str: The mime type of the file.
    """

    # used for relative paths
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    root_path = os.path.dirname(os.path.join(cwd, rel_path))

    if not os.path.isabs(file_path):
        file_path = os.path.join(root_path, file_path)

    return magic.from_file(str(file_path), mime=True)


def get_buffer_type(buffer: bytes) -> str:
    """
    Return the buffer mime type with mime type
    see full list here : https://developer.mozilla.org/fr/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types

    Args:
        buffer (bytes): The buffer to analyze.

    Returns:
        str: The mime type of the buffer.
    """

    return magic.from_buffer(buffer, mime=True)


def random_string(lenght: int = 10) -> str:
    """
    Generate a random string composed of lower cased ascii characters

    Args:
        lenght (int): The length of the string to generate. (default: 10)

    Returns:
        str: random string of lenght `lenght` composed of lower cased ascii characters
    """

    letters = string.ascii_lowercase

    generated_random_string = "".join(random.choice(letters) for _ in range(lenght))

    return generated_random_string


def create_random_directory(root_path: str) -> str:
    """
    Create a random directory in the root_path.

    Args:
        root_path (str): The path to the root directory to create the file into.

    Returns:
        str: The path to the created directory.
    """
    full_path = os.path.join(root_path, random_string(10))
    create_directory(full_path)

    return full_path


def generate_random_filename(root_path: str, extension: str) -> Tuple[str, str]:
    """
    Generate a random filename in the root_path.

    Args:
        root_path (str): The path to the root directory to create the file into.
        extension (str): The extension of the file.

    Returns:
        str: The path to the created file.
    """

    filename = ".".join([random_string(10), extension])
    full_path = os.path.join(root_path, filename)

    return full_path, filename

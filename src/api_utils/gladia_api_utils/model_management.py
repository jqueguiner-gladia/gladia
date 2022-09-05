import os
import shutil
import sys
import tempfile
import threading
from logging import getLogger
from pathlib import Path
from urllib.parse import urlparse

from git import Repo

from .file_management import download_file, is_uncompressable, uncompress, get_tmp_filename, delete_directory

logger = getLogger(__name__)


def download_model(
    url: str,
    output_path: str,
    uncompress_after_download: bool = True,
    file_type: str = None,
    reset: bool = True,
    branch: str = "origin",
) -> str:
    """
    Download a model and uncompress it if necessary
    reset lets you decide not to force sync between huggingface hub and you local repo (for testing purposes for instance)

    Args:
        url (str): url of the model to download
        output_path (str): path to download the model to
        uncompress_after_download (bool): whether to uncompress the model after download (default: True)
        file_type (str): type of file to download (if None, will try to guess)
        reset (bool): whether to reset the local repo (default: True)
        branch (str): branch to use (default: origin)

    Returns:
        str: path to the downloaded model
    """

    namespace = sys._getframe(1).f_globals
    
    rel_path = namespace["__file__"]

    # check env to see if mutualized_storage had been set
    mutualized_storage_root = os.getenv("GLADIA_TMP_MODEL_PATH", "/tmp/gladia/models/")

    if not os.path.isabs(output_path):
        output_path = os.path.join(mutualized_storage_root, rel_path, output_path)

    logger.debug(f"Downloading model from {url} to {output_path}")

    domain = urlparse(url).netloc

    # if domain is huggingface
    # and if its not a file (resolve) but a git-lfs repo
    # else if (resolve) or not huggingface consider url as a file
    if "huggingface.co" in domain and "/resolve/" not in url:
        # check if directory exists if not clone it else pull
        os.environ["GIT_LFS_SKIP_SMUDGE"] = "1"

        if not os.path.isdir(Path(output_path)):
            logger.debug(f"Cloning HuggingFace Model from {url}")
            Repo.clone_from(url, output_path)
            os.system(f"cd {output_path} && git lfs pull")

        else:
            if reset:
                logger.debug(f"Pulling HuggingFace Model from {url}")
                repo = Repo(output_path)
                repo.git.reset("--hard", "origin/main")
                os.system(f"cd {output_path} && git lfs pull")

    else:
        logger.debug(f"Downloading {url}")

        # if the output_path is not an existing directory create it
        if not os.path.exists(Path(output_path)):
            os.makedirs(output_path, exist_ok=True)

        # create a temporary folder to download the model to
        dl_tmp_dirpath = get_tmp_filename()
        uncompress_tmp_dirpath = get_tmp_filename()

        logger.debug(f"Temporary directory for download: {dl_tmp_dirpath}")
        
        downloaded_full_path = download_file(url=url, file_full_path=dl_tmp_dirpath, force_create_dir=True, force_redownload=False)
        logger.debug(f"Downloaded model to {downloaded_full_path}")
        # if the model is uncompressable uncompress it
        if uncompress_after_download and is_uncompressable(str(downloaded_full_path)):
            logger.debug("Uncompressing {downloaded_full_path} to {output_path}")
            uncompress(path=downloaded_full_path, destination=uncompress_tmp_dirpath, delete_after_uncompress=True)
            os.system(f"mv {uncompress_tmp_dirpath}/* {output_path}")
        else:
            os.system(f"mv {dl_tmp_dirpath}/* {output_path}")

        # clean up temporary folder
        delete_directory(dl_tmp_dirpath)
        delete_directory(uncompress_tmp_dirpath)

    return output_path


def download_models(model_list: dict) -> dict:
    """
    Download a list of models and uncompress them if necessary

    Args:
        model_list (dict): list of models to download should be [(url, output_path, uncompression_mode)]

    Returns:
        dict: list of models with their paths
    """

    # manage relative imports
    namespace = sys._getframe(1).f_globals
    cwd = os.getcwd()
    rel_path = namespace["__file__"]
    rel_path = rel_path.lstrip("./")
    if ".py" in rel_path:
        rel_path = os.path.dirname(rel_path)

    # used in case of relative path
    model_root_path = os.path.dirname(os.path.join(cwd, rel_path))

    logger.debug("Downloading multiple models")
    threads = []
    output = dict()

    # check env to see if mutualized_storage had been set
    mutualized_storage = os.getenv("MODEL_CACHE_ROOT", True)
    mutualized_storage_root = os.getenv("MODEL_CACHE_ROOT", "/tmp/gladia/models/")

    for key, model in model_list.items():
        if not os.path.isabs(model["output_path"]):
            if mutualized_storage:
                model["output_path"] = os.path.join(
                    mutualized_storage_root, rel_path, model["output_path"]
                )
            else:
                model["output_path"] = os.path.join(
                    model_root_path, model["output_path"]
                )

            t = threading.Thread(
                target=download_model, args=(model["url"], model["output_path"])
            )
            output[key] = model
            threads.append(t)
            t.start()

    for t in threads:
        t.join()

    return output

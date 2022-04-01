import os
import apis
import json
import logging
import pkgutil
import importlib

from icecream import ic
from fastapi import FastAPI
from pattern.text.en import singularize
from fastapi.responses import ORJSONResponse
from fastapi.middleware.cors import CORSMiddleware
from fastapi_utils.timing import add_timing_middleware
from prometheus_fastapi_instrumentator import Instrumentator


def __init_config() -> dict:
    """
    Load config file and return it as a dict.

    Default path is `config.json`, use API_CONFIG_FILE varenv to change it.

    :return: config dict
    """

    config_file = os.getenv('API_CONFIG_FILE', 'config.json')

    if os.path.isfile(config_file):
        with open("config.json", "r") as f:
            return json.load(f)


def __init_logging(api_config: dict) -> logging.Logger:
    """
    Create a logging.Logger with it format set to config["logs"]["log_format"] f exist, else default.

    :param api_config: api config dict
    :return: logger initialized
    """

    try:
        logging.basicConfig(level=logging.INFO, format=api_config["logs"]["log_format"])
    except KeyError:
        logging.basicConfig(level=logging.INFO)

    return logging.getLogger(__name__)


def __init_prometheus_instrumentator(instrumentator_config: dict) -> Instrumentator:
    """
    Initialize the prometheus_fastapi_instrumentator.Instrumentator using api config dict
    :param instrumentator_config: instrumentator config
    :return: Initialized Instrumentator
    """

    return Instrumentator(
        should_group_status_codes=instrumentator_config["should_group_status_codes"],
        should_ignore_untemplated=instrumentator_config["should_ignore_untemplated"],
        should_group_untemplated=instrumentator_config["should_group_untemplated"],
        should_respect_env_var=instrumentator_config["should_respect_env_var"],
        env_var_name=instrumentator_config["env_var_name"],
        excluded_handlers=instrumentator_config["excluded_handlers"],
        should_round_latency_decimals=instrumentator_config["should_round_latency_decimals"],
        round_latency_decimals=instrumentator_config["round_latency_decimals"],
        should_instrument_requests_inprogress=instrumentator_config["should_instrument_requests_inprogress"],
        inprogress_name=instrumentator_config["inprogress_name"],
        inprogress_labels=instrumentator_config["inprogress_labels"],
    )


config = __init_config()
logger = __init_logging(config)
app = FastAPI(default_response_class=ORJSONResponse)

if config["prometheus"]["active"]:
    instrumentator = __init_prometheus_instrumentator(config["prometheus"]["instrumentator"])


if config["logs"]["timing_activated"]:
    add_timing_middleware(app, record=logger.info, prefix="app")


app.add_middleware(
    CORSMiddleware,
    allow_origins=config["CORS"]["allow_origins"],
    allow_credentials=config["CORS"]["allow_credentials"],
    allow_methods=config["CORS"]["allow_methods"],
    allow_headers=config["CORS"]["allow_headers"],
)


def import_submodules(package, recursive=True):
    global active_tasks

    if isinstance(package, str):
        current_package = package.split(".")
        package = importlib.import_module(package)

    for loader, name, is_pkg in pkgutil.walk_packages(package.__path__):
        full_name = package.__name__ + "." + name
        this_module = importlib.import_module(full_name)
        module_short_name = full_name.replace("apis", "")[1:]

        if "router" in dir(this_module):
            module_input, module_output, module_task = module_short_name.split(".")
            module_task = singularize(module_task).upper()
            active_task_list = list(
                map(
                    lambda each: singularize(each).upper(),
                    config["active_tasks"][module_input][module_output],
                )
            )

            if "None".upper() not in active_task_list and (
                "*" in config["active_tasks"][module_input][module_output]
                or module_task in active_task_list
            ):

                module_prefix = full_name.replace(".", "/").replace("apis", "")
                ic(f"Loading module: {full_name}")
                app.include_router(this_module.router, prefix=module_prefix)

        if recursive and is_pkg:
            ic(module_short_name)
            module_split = module_short_name.split(".")

            if len(module_split) == 1:
                import_submodules(full_name)
            elif (
                len(module_split) == 2
                and "None".upper
                not in map(
                    lambda each: each.upper(),
                    config["active_tasks"][module_split[0]][module_split[1]],
                )
                or len(config["active_tasks"][module_split[0]][module_split[1]]) == 0
            ):
                ic(f"importing {full_name}")
                import_submodules(full_name)
            elif len(module_split) == 3 and (
                module_split[2].rstrip("s")
                in map(
                    lambda each: each.rstrip("s"),
                    config["active_tasks"][module_split[0]][module_split[1]],
                )
                or "*" in config["active_tasks"][module_split[0]][module_split[1]]
            ):
                ic(f"importing {full_name}")
                import_submodules(full_name)
            elif len(module_split) == 4:
                ic(f"importing {full_name}")
                import_submodules(full_name)
            else:
                ic(f"skipping {module_short_name}")


import_submodules(apis)

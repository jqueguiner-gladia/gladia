"""
Notes:
Dans le cas où un modèle triton n'a pas encore été DL (car jamais call et lazy load)
le triton manager ne le trouvera pas et ça sera au TritonClient de le load (ce qui pourrait faire un OOM)
"""

import os
import json
import redis
import requests
import tritonclient.http as httptritonclient

from time import time
from typing import Dict
from logging import getLogger


logger = getLogger(__name__)


class SingletonMeta(type):
    """
    The Singleton class can be implemented in different ways in Python. Some
    possible methods include: base class, decorator, metaclass. We will use the
    metaclass because it is best suited for this purpose.
    """

    _instances = {}

    def __call__(cls, *args, **kwargs):
        """
        Possible changes to the value of the `__init__` argument do not affect
        the returned instance.
        """
        if cls not in cls._instances:
            instance = super().__call__(*args, **kwargs)
            cls._instances[cls] = instance
        return cls._instances[cls]


class TritonManager(metaclass=SingletonMeta):
    """Wrapper suggaring triton'client usage"""

    def __init__(
        self,
        **kwargs,
    ) -> None:
        """TritonManager's initializer
        """

        self.__TRITON_MODELS_PREFIX = "triton-models"

        self.__triton_server_url = kwargs.get(
            "triton_server_url",
            os.getenv("TRITON_SERVER_URL", default="localhost:8000"),
        )

        self.__triton_client = httptritonclient.InferenceServerClient(
            url=self.__triton_server_url,
            verbose=False
        )

        self.__redis_client = redis.Redis(
            host=os.getenv("REDIS_HOST", 'localhost'),
            port=os.getenv("REDIS_PORT", 6379),
            db=os.getenv("REDIS_DB", 0),
            decode_responses=True
        )

    def is_server_live(self, **kwargs) -> bool:
        return self.__triton_client.is_server_live(
            query_params=kwargs.get("query_params", {})
        )

    def is_server_ready(self, **kwargs) -> bool:
        return self.__triton_client.is_server_ready(
            query_params=kwargs.get("query_params", {})
        )

    def load_model(self, model: str) -> None:
        return self.__triton_client.load_model(model)

    def unload_model(self, model: str) -> None:
        return self.__triton_client.unload_model(model)

    def is_model_ready(self, model: str) -> bool:
        return self.__triton_client.is_model_ready(model)

    def is_model_running(self, model: str) -> bool:
        return self.__get_model_informations(model)["running"]

    def is_model_releasable(self, model: str) -> bool:
        return self.__get_model_informations(model)["locked_in_memory"] == False

    def get_model_metadata(self, model: str, **kwargs) -> bool:
        return self.__triton_client.get_model_metadata(
            model,
            query_params=kwargs.get("query_params", {})
        )

    def get_models_in_registery(self):
        """Returns a short description of every models presents in the model registery.

        Returns:
            List[Dict[str, str]]: [{'name': 'NAME', 'version': 'VERSION', 'state': 'STATE'}, ...]
        """

        return self.__triton_client.get_model_repository_index()

    def __create_default_memory_information(self, **kwargs):
        return {
            "running": kwargs.get("default_value_running", False),
            "locked_in_memory": kwargs.get("default_value_locked_in_memory", False),
            "last_call": kwargs.get("default_value_last_call", 0),
            "use_by": kwargs.get("default_value_use_by", []),
            "place_in_memory_queue": kwargs.get("default_value_place_in_memory_queue", -1),
        }

    def __get_model_informations(self, model_name, **kwargs):
        if f"{self.__TRITON_MODELS_PREFIX}:{model_name}" not in self.__redis_client.keys(f"{self.__TRITON_MODELS_PREFIX}:*"):
            return self.__create_default_memory_information(**kwargs)
        else:
            return json.loads(self.__redis_client.get(
                name=f"{self.__TRITON_MODELS_PREFIX}:{model_name}",
            ))

    def add_model_to_memory_queue(self, model, first_in_queue=False):
        model_informations = self.__get_model_informations(model)

        model_informations["place_in_memory_queue"] = 0 if first_in_queue else time()

        self.__redis_client.set(
            name=f"{self.__TRITON_MODELS_PREFIX}:{model}",
            value=json.dumps(model_informations)
        )

    def remove_model_from_memory_queue(self, model):
        model_informations = json.loads(self.__redis_client.get(
            name=f"{self.__TRITON_MODELS_PREFIX}:{model}",
        ))

        model_informations["place_in_memory_queue"] = -1

        self.__redis_client.set(
            name=f"{self.__TRITON_MODELS_PREFIX}:{model}",
            value=json.dumps(model_informations),
        )        

    def get_memory_queue(self):
        models_in_redis = self.__redis_client.keys("{self.__TRITON_MODELS_PREFIX}:*")

        if not models_in_redis:
            return []

        pipe = self.__redis_client.pipeline()

        for model in models_in_redis:
            pipe.get(model)

        models_informations = dict(zip(models_in_redis, pipe.execute()))
        models_informations = {model: informations for (model, informations) in models_informations if models_informations[model]["place_in_memory_queue"] >= 0}

        return list(models_informations.keys()).sort(key=lambda model : models_informations[model])

    def update_model_locked_in_memory_value(self, model: str, is_locked: bool) -> None:
        if not isinstance(is_locked, bool):
            raise RuntimeError("is_running parameter must be of type bool")

        model_informations = self.__get_model_informations(model)

        model_informations["locked_in_memory"] = is_locked

        self.__redis_client.set(
            name=f"{self.__TRITON_MODELS_PREFIX}:{model}",
            value=json.dumps(model_informations)
        )

    def add_manager_who_is_using_model(self, model_using: str, model_used: str) -> None:
        model_informations = self.__get_model_informations(model_used)

        model_informations["use_by"].append(model_using)
        model_informations["use_by"] = list(set(model_informations["use_by"]))

        self.__redis_client.set(
            name=f"{self.__TRITON_MODELS_PREFIX}:{model_used}",
            value=json.dumps(model_informations)
        )

    def remove_manager_who_is_no_longer_using_model(self, model_using: str, model_used: str) -> None:
        model_informations = self.__get_model_informations(model_used)

        model_informations["use_by"].remove(model_using)

        self.__redis_client.set(
            name=f"{self.__TRITON_MODELS_PREFIX}:{model_used}",
            value=json.dumps(model_informations)
        )

    def get_model_managers(self, model: str) -> None:
        model_informations = self.__get_model_informations(model)

        return model_informations["use_by"]

    def update_model_running_status(self, model: str, is_running: bool) -> None:
        if not isinstance(is_running, bool):
            raise RuntimeError("is_running parameter must be of type bool")

        model_informations = self.__get_model_informations(model)

        model_informations["running"] = is_running

        if is_running:
            model_informations["last_call"] = time()

        self.__redis_client.set(
            name=f"{self.__TRITON_MODELS_PREFIX}:{model}",
            value=json.dumps(model_informations)
        )

    # TODO: remove
    def _debug_get_models_informations(self):
        models_in_redis = self.__redis_client.keys("{self.__TRITON_MODELS_PREFIX}:*")

        pipe = self.__redis_client.pipeline()

        for model in models_in_redis:
            pipe.get(model)

        return dict(zip(models_in_redis, pipe.execute()))

    def __get_total_gpus_memory(self) -> Dict[str, float]:
        gpus = {}

        response = requests.get(f"http://{self.__triton_server_url.split(':')[0]}:8002/metrics")

        if response.status_code != 200:
            logger.error("Couldn't access metrics for the triton server, is '--allow-gpu-metrics' set to `true`?")

            return {}

        response_content = str(response.text)

        for line in response_content.split("\n"):

            if "nv_gpu_memory_total_bytes" not in line:
                continue

            if "gpu_uuid=" not in line:
                continue

            gpu_uuid = line.split("gpu_uuid=")[1] # "GPU-SOME-RANDOM-ID"} 48008.672000
            gpu_uuid = gpu_uuid.split("\"}")[0] # "GPU-SOME-RANDOM-ID
            gpu_uuid = gpu_uuid[1:] # GPU-SOME-RANDOM-ID

            gpus[gpu_uuid] = float(line.split(" ")[1])

        if len(gpus) == 0:
            logger.error("Couldn't found nv_gpu_memory_total_bytes in metrics, response_content: ", response_content)

        return gpus

    def __get_used_gpus_memory(self) -> Dict[str, float]:
        gpus = {}

        response = requests.get(f"http://{self.__triton_server_url.split(':')[0]}:8002/metrics")

        if response.status_code != 200:
            logger.error("Couldn't access metrics for the triton server, is '--allow-gpu-metrics' set to `true`?")

            return {}

        response_content = str(response.text)

        for line in response_content.split("\n"):
            if "nv_gpu_memory_used_bytes" not in line:
                continue

            if "gpu_uuid=" not in line:
                continue

            gpu_uuid = line.split("gpu_uuid=")[1] # "GPU-SOME-RANDOM-ID"} 48008.672000
            gpu_uuid = gpu_uuid.split("\"}")[0] # "GPU-SOME-RANDOM-ID
            gpu_uuid = gpu_uuid[1:] # GPU-SOME-RANDOM-ID

            gpus[gpu_uuid] = float(line.split(" ")[1])

        if len(gpus) == 0:
            logger.error("Couldn't found nv_gpu_memory_used_bytes in metrics, response_content: ", response_content)

        return gpus

    def get_gpus_usage(self):
        total_gpus_memory = self.__get_total_gpus_memory()
        used_gpus_memory = self.__get_used_gpus_memory()

        if total_gpus_memory.keys() != used_gpus_memory.keys():
            logger.error(f"total_gpus_memory and used_gpus_memory have different keys.\ntotal_gpus_memory.keys():{total_gpus_memory.keys()}\nused_gpus_memory.keys():{used_gpus_memory.keys()}")

            return {}

        gpus_usage = {}

        for gpu_uuid, total_memory in total_gpus_memory.items():
            gpus_usage[gpu_uuid] = used_gpus_memory[gpu_uuid] / total_memory

        return gpus_usage

"""

triton-models:model = {
    "running": bool,
    "last_call": time,
    "use_by": [str],
    "locked_in_memory": bool,
    "place_in_memory_queue": time,
}

Steps:
1. Changer la valeur de triton-models:model pour suivre le dict décrit au-dessus                                                                                        STATUS : DONE
2. Faire que les variables "last_call" et "running" soient updated à chaque run                                                                                         STATUS : DONE
3. Prendre en compte que certains modèles soient de type "preloaded"                                                                                                    STATUS : DONE
4. Faire en sorte que si un modèle est composé de sub-modèle alors ne pas unload les sub-modèles si celui-ci est running                                                STATUS : DONE
5. Handle le fais qu'en prod je ne peux pas faire nvdia-smi pour savoir la VRAM libre sur l'instance Triton (/metrics ?)                                                STATUS : DONE
6. Passer en full synch au-lieu de plusieurs TritonManager travaillant en async en mm temps                                                                             STATUS : TODO
"""
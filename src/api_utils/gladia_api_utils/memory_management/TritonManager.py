"""
FIXME:
Dans le cas où un modèle triton n'a pas encore été DL (car jamais call et lazy load)
le triton manager ne le trouvera pas et ça sera au TritonClient de le load (ce qui pourrait faire un OOM)
"""

import os
import shutil
from logging import getLogger
from typing import Any, Dict, List

import requests
import tritonclient.http as httptritonclient
import uvicorn
from dto import ModelStatus
from git.repo.base import Repo
from IManagerServer import IManagerServer


class TritonManager(IManagerServer):
    """Wrapper suggaring triton'client usage"""

    __logger = getLogger(__name__)

    # TODO: fill every other possible status value
    __MODEL_STATUS = {
        "READY": ModelStatus.READY,
    }

    def __init__(
        self,
        **kwargs,
    ) -> None:
        """TritonManager's initializer"""

        super().__init__(title=__name__)

        self.__triton_server_url = kwargs.get(
            "triton_server_url",
            os.getenv("TRITON_SERVER_URL", default="localhost:8000"),
        )

        self.__triton_client = httptritonclient.InferenceServerClient(
            url=self.__triton_server_url, verbose=False
        )

        self.__models = {}

        @self.get("/memory")
        def return_memory():
            return self.get_available_memory()

        @self.post("/{model}/init")
        def download_model(model: str, params: Dict[str, Any]):
            self.__models[model] = params["main_model"]

            clone_to_path = f"/tmp/triton-model-cloned-{model}"

            if os.path.exists(clone_to_path):
                return

            Repo.clone_from(params["git"], clone_to_path)

            triton_models_dir = os.getenv("TRITON_MODELS_PATH", "/tmp/triton")

            already_downloaded_models = os.listdir(triton_models_dir)

            for filename in os.listdir(clone_to_path):
                if filename[0] == "." or filename in already_downloaded_models:
                    self.__logger.debug(
                        f"{filename} not moved to {triton_models_dir} as it's either a hidden file or it does already exist in target path"
                    )

                    continue

                shutil.move(os.path.join(clone_to_path, filename), triton_models_dir)

            shutil.rmtree(clone_to_path)
            open(
                clone_to_path, "w"
            ).close()  # NOTE: then, if the server is restarted it see that clone_to_path already exists and doesn't need to be cloned again

        @self.post("/{model}/load")
        def load_model(model: str):
            return self.load_model(self.__models.get(model, model))

        @self.post("/{model}/unload")
        def unload_model(model: str):
            return self.unload_model(self.__models.get(model, model))

        @self.get("/{model}/unloadable")
        def unload_model(model: str):
            return self.model_can_be_unloaded(self.__models.get(model, model))

        @self.get("/{model}/loaded")
        def is_model_loaded(model: str):
            return self.model_is_loaded(self.__models.get(model, model))

        @self.get("/models/")
        def return_managed_models():
            return self.get_managed_models()

    def init_model(self, model: str, params: Dict[str, Any]) -> None:
        pass

    def model_is_loaded(self, model: str) -> bool:
        model_repository = self.__triton_client.get_model_repository_index()

        if model not in model_repository:
            return False

        model_status = model_repository[model_repository.index(model)].get(
            "state", ModelStatus.NOT_READY
        )

        if model_status == ModelStatus.READY:
            return True

        return False

    def get_available_memory(self):
        return [1 - gpu_usage for gpu_usage in list(self.__get_gpus_usage().values())]

    def load_model(self, model: str) -> None:
        return self.__triton_client.load_model(model)

    def unload_model(self, model: str) -> None:
        return self.__triton_client.unload_model(model)

    def model_can_be_unloaded(self, model: str) -> bool:
        # TODO
        raise NotImplementedError

    def get_managed_models(self) -> List[Dict[str, str]]:
        """Retrieve all the model managed by this manager

        Returns:
            List[Dict[str, str]]: list of managed models [{'name': 'NAME', 'version': 'VERSION', 'state': 'STATE'}, ...]
        """

        model_repository = self.__triton_client.get_model_repository_index()

        for model in model_repository:
            if "state" not in model_repository[model].keys():
                model_repository[model]["state"] = ModelStatus.NOT_READY
            else:
                model_repository[model]["state"] = self.__MODEL_STATUS.get(
                    model_repository[model]["state"]
                )

        return model_repository

    def __is_server_live(self, **kwargs) -> bool:
        return self.__triton_client.is_server_live(
            query_params=kwargs.get("query_params", {})
        )

    def __is_server_ready(self, **kwargs) -> bool:
        return self.__triton_client.is_server_ready(
            query_params=kwargs.get("query_params", {})
        )

    def __is_model_ready(self, model: str) -> bool:
        return self.__triton_client.is_model_ready(model)

    def __get_model_metadata(self, model: str, **kwargs) -> bool:
        return self.__triton_client.get_model_metadata(
            model, query_params=kwargs.get("query_params", {})
        )

    def __get_total_gpus_memory(self) -> Dict[str, float]:
        gpus = {}

        response = requests.get(
            f"http://{self.__triton_server_url.split(':')[0]}:8002/metrics"
        )

        if response.status_code != 200:
            self.__logger.error(
                "Couldn't access metrics for the triton server, is '--allow-gpu-metrics' set to `true`?"
            )

            return {}

        response_content = str(response.text)

        for line in response_content.split("\n"):

            if "nv_gpu_memory_total_bytes" not in line:
                continue

            if "gpu_uuid=" not in line:
                continue

            gpu_uuid = line.split("gpu_uuid=")[1]  # "GPU-SOME-RANDOM-ID"} 48008.672000
            gpu_uuid = gpu_uuid.split('"}')[0]  # "GPU-SOME-RANDOM-ID
            gpu_uuid = gpu_uuid[1:]  # GPU-SOME-RANDOM-ID

            gpus[gpu_uuid] = float(line.split(" ")[1])

        if len(gpus) == 0:
            self.__logger.error(
                "Couldn't found nv_gpu_memory_total_bytes in metrics, response_content: ",
                response_content,
            )

        return gpus

    def __get_used_gpus_memory(self) -> Dict[str, float]:
        gpus = {}

        response = requests.get(
            f"http://{self.__triton_server_url.split(':')[0]}:8002/metrics"
        )

        if response.status_code != 200:
            self.__logger.error(
                "Couldn't access metrics for the triton server, is '--allow-gpu-metrics' set to `true`?"
            )

            return {}

        response_content = str(response.text)

        for line in response_content.split("\n"):
            if "nv_gpu_memory_used_bytes" not in line:
                continue

            if "gpu_uuid=" not in line:
                continue

            gpu_uuid = line.split("gpu_uuid=")[1]  # "GPU-SOME-RANDOM-ID"} 48008.672000
            gpu_uuid = gpu_uuid.split('"}')[0]  # "GPU-SOME-RANDOM-ID
            gpu_uuid = gpu_uuid[1:]  # GPU-SOME-RANDOM-ID

            gpus[gpu_uuid] = float(line.split(" ")[1])

        if len(gpus) == 0:
            self.__logger.error(
                "Couldn't found nv_gpu_memory_used_bytes in metrics, response_content: ",
                response_content,
            )

        return gpus

    def __get_gpus_usage(self):
        total_gpus_memory = self.__get_total_gpus_memory()
        used_gpus_memory = self.__get_used_gpus_memory()

        if total_gpus_memory.keys() != used_gpus_memory.keys():
            self.__logger.error(
                f"total_gpus_memory and used_gpus_memory have different keys.\ntotal_gpus_memory.keys():{total_gpus_memory.keys()}\nused_gpus_memory.keys():{used_gpus_memory.keys()}"
            )

            return {}

        gpus_usage = {}

        for gpu_uuid, total_memory in total_gpus_memory.items():
            gpus_usage[gpu_uuid] = used_gpus_memory[gpu_uuid] / total_memory

        return gpus_usage


if __name__ == "__main__":
    triton_manager = TritonManager()

    uvicorn.run(
        triton_manager,
        host=os.getenv("TRITON_MANAGER_HOST", "0.0.0.0"),
        port=int(os.getenv("TRITON_MANAGER_PORT", "8011")),
    )

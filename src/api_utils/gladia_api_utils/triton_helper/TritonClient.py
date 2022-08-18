import os
import pathlib
import sys
import requests
import tritonclient.http as tritonclient

from typing import Any
from time import sleep
from warnings import warn
from logging import getLogger

from .TritonManager import TritonManager
from .download_active_models import download_triton_model


logger = getLogger(__name__)


class TritonClient:
    """Wrapper suggaring triton'client usage"""

    MAX_VRAM_USAGE: float = 0.5

    def __init__(
        self,
        model_name: str,
        **kwargs,
    ) -> None:
        """TritonClient's initializer

        Args:
            model_name (str): name of the model to communicate with
            current_path (str, optional): current path (allows to download model if needed). Defaults to "".
        """


        self.__triton_manager = TritonManager()

        self.__triton_server_url = kwargs.get(
            "triton_server_url",
            os.getenv("TRITON_SERVER_URL", default="localhost:8000"),
        )

        self.__current_path = kwargs.get(
            "current_path",
            str(
                pathlib.Path(sys._getframe(1).f_globals["__file__"])
                .parents[0]
                .absolute()
            ),
        )

        self.SLEEP_TIME_IN_QUEUE = 0.02
        self.SLEEP_TIME_NO_MEMORY = 0.01

        self.__model_name = model_name
        self.__model_sub_parts = kwargs.get("sub_parts", [])

        self.__preload_model: bool = kwargs.get("preload_model", False)

        self.__triton_manager.update_model_locked_in_memory_value(self.__model_name, is_locked=True)

        for sub_part in self.__model_sub_parts:
            self.__triton_manager.update_model_locked_in_memory_value(sub_part, is_locked=True)

        if os.getenv("TRITON_MODELS_PATH") == "":
            warn(
                "[DEBUG] TRITON_MODELS_PATH is not set, please specify it in order to be able to download models."
            )

        self.__download_model(os.path.join(self.__current_path, ".git_path"))

        if self.__preload_model and not self.load_model():
            logger.error(
                f"{self.__model_name} has not been properly loaded. Setting back lazy load to True"
            )

            self.__preload_model = False

        self.__client = tritonclient.InferenceServerClient(
            url=self.__triton_server_url, verbose=False
        )

        self.__registered_inputs = {}

        self.__registered_outputs = [
            tritonclient.InferRequestedOutput(
                name=kwargs.get("output_name", "output__0")
            )
        ]

    @property
    def client(self):
        return self.__client

    def load_model(self) -> bool:
        """Requests triton to load the model

        Returns:
            bool: whether the model has been successfully loaded or not
        """

        for model_sub_part in self.__model_sub_parts:
            response = requests.post(
                url=f"http://{self.__triton_server_url}/v2/repository/models/{model_sub_part}/load",
            )

            if response.status_code != 200:
                return False

        response = requests.post(
            url=f"http://{self.__triton_server_url}/v2/repository/models/{self.__model_name}/load"
        )

        return response.status_code == 200

    def set_input(self, shape, datatype: str, **kwargs) -> None:
        """Add a new input to the triton inferer. Each input has to be registered before usage.

        Args:
            shape (int, ...): shape of the input to register
            datatype (str): datatype of the input to register
        """

        input_name = kwargs.get("name", f"input__{len(self.__registered_inputs)}")
        self.__registered_inputs[input_name] = tritonclient.InferInput(
            name=input_name, shape=shape, datatype=datatype
        )

    def unset_input(self, input_name: str):
        del self.__registered_inputs[input_name]

    def register_new_output(self, **kwargs) -> None:
        """Add a new output to the triton inferer. Each ouput has to be registered before usage.\n
        By default one ouput named `output__0` is already registered.
        """

        self.__registered_outputs.append(
            tritonclient.InferRequestedOutput(
                name=kwargs.get("name", f"ouput__{len(self.__registered_outputs)}")
            )
        )

    def __download_model(self, path_to_git_path_file: str, sleep_time: int = 0) -> None:
        """Check if the model need to be downloaded, if so download and extract it.

        Args:
            path_to_git_path_file (str): path to the `.git_path` file
            sleep_time (int, optional): sleep time after extracting the model. Defaults to 0.
        """

        response = requests.post(
            url=f"http://{self.__triton_server_url}/v2/repository/index"
        )

        for model in response.json():
            if model["name"] == self.__model_name:
                return

        warn(
            "Downloading model from hugging-face, to prevent lazy downloading please specify TRITON_LAZY_DOWNLOAD=False"
        )

        download_triton_model(
            triton_models_dir=os.getenv("TRITON_MODELS_PATH"),
            git_path=path_to_git_path_file,
        )

        sleep(sleep_time)

    def __call__(self, *args, **kwds) -> [Any]:
        """Call the triton inferer with the inputs in `args`.

        Returns:
            [Any]: List of outputs from the model
        """

        self.triton_manager_pre_hook(self.__model_name)

        for arg, registered_input in zip(args, self.__registered_inputs.values()):
            registered_input.set_data_from_numpy(arg)

        need_to_load_model = True

        if self.__preload_model or str(kwds.get("load_model", "")).lower() == "false":
            need_to_load_model = False

        if need_to_load_model and not self.load_model():
            logger.error(
                f"{self.__model_name} has not been properly loaded. Returning empty response"
            )

            return [[]]

        model_response = self.client.infer(
            self.__model_name,
            model_version="1",
            inputs=self.__registered_inputs.values(),
            outputs=self.__registered_outputs,
        )

        self.triton_manager_post_hook(self.__model_name)

        return [
            model_response.as_numpy(output.name()).tolist()
            for output in self.__registered_outputs
        ]

    def _there_is_not_enought_vram_available(self) -> bool:
        return all(vram_usage >= self.MAX_VRAM_USAGE for vram_usage in list(self.__triton_manager.get_gpus_usage().values()))

    def __another_model_if_first_in_queue(self, model):
        memory_queue = self.__triton_manager.get_memory_queue()

        if len(memory_queue) == 0:
            return False

        if memory_queue[0] != model:
            return True

        return False

    def triton_manager_pre_hook(self, model):

        if self._there_is_not_enought_vram_available() or self.__another_model_if_first_in_queue(model):
            print(model, "JOINING THE QUEUE!")

            self.__triton_manager.add_model_to_memory_queue(model)

        if self.__another_model_if_first_in_queue(model) and self.__triton_manager.is_model_ready(model) is True and self.__preload_model == False:
                self.__triton_manager.unload_model(model)

        while self.__another_model_if_first_in_queue(model):
            print(model, "I'M WAITING IN THE QUEUE:", self.__triton_manager.get_memory_queue())

            sleep(self.SLEEP_TIME_IN_QUEUE)

        memory_queue = self.__triton_manager.get_memory_queue()

        if len(memory_queue) > 0 and model not in memory_queue or self._there_is_not_enought_vram_available():
            self.__triton_manager.add_model_to_memory_queue(model, first_in_queue=True)

        while self._there_is_not_enought_vram_available():
            print(model, "WAITING FOR MORE MEMORY!:",
                self.__triton_manager.get_gpus_usage(),
                self.__triton_manager.get_models_in_registery(),
            )

            models_loaded = [model_in_registery["name"] for model_in_registery in self.__triton_manager.get_models_in_registery() if ("state" in model_in_registery.keys() and model_in_registery["state"] == "READY")]
            models_than_can_be_unloaded = [model_loaded for model_loaded in models_loaded if (self.__triton_manager.is_model_running(model) is False and self.__triton_manager.is_model_releasable(model) is True and len(self.__triton_manager.get_model_managers(model_loaded)) == 0)]

            if len(models_than_can_be_unloaded) == 0:
                logger.error("Requesting to unload a triton model but there is no triton model to unload, this could result in a infinit loop\n" + 
                f"models_loaded: {models_loaded}\n" +
                f"models_than_can_be_unloaded: {models_than_can_be_unloaded}"
                )

            else:
                print("Unloading:", models_than_can_be_unloaded[0])

                self.__triton_manager.unload_model(models_than_can_be_unloaded[0])

            sleep(self.SLEEP_TIME_NO_MEMORY)

        self.__triton_manager.load_model(model)

        for sub_part in self.__model_sub_parts:
            self.__triton_manager.add_manager_who_is_using_model(model_using=self.__model_name, model_used=sub_part)
            self.__triton_manager.update_model_running_status(sub_part, is_running=True)

        if model in self.__triton_manager.get_memory_queue():
            self.__triton_manager.remove_model_from_memory_queue(model)

        return

    def triton_manager_post_hook(self, model):
        self.__triton_manager.update_model_running_status(model, is_running=False)

        for sub_part in self.__model_sub_parts:
            self.__triton_manager.remove_manager_who_is_no_longer_using_model(model_using=self.__model_name, model_used=sub_part)

            if len(self.__triton_manager.get_model_managers(sub_part)) == 0:
                self.__triton_manager.update_model_running_status(sub_part, is_running=False)

        # NOTE: on ne fait pas d'unload ici pour limiter les call asynch en //


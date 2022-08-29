import requests

from typing import List, Dict
from IManager import IManager
from dto import ModelType


class ManagerServerHandler(IManager):
    def __init__(self, server_url: str, server_port: str, model_type: ModelType):
        self.__server_endpoint = f"http://{server_url}:{server_port}"

        self.__MODEL_TYPE = model_type

    def get_available_memory(self) -> List[float]:
        endpoint = f"{self.__server_endpoint}/memory/"

        response = requests.get(endpoint)

        return response.json()

    def load_model(self, model: str) -> None:
        endpoint = f"{self.__server_endpoint}/{model}/load/"

        response = requests.post(endpoint)

        print(response.status_code, "22" * 100)

        return response.json()

    def unload_model(self, model: str) -> None:
        endpoint = f"{self.__server_endpoint}/{model}/unload/"

        response = requests.post(endpoint)

        return response.json()

    def model_can_be_unloaded(self, model: str) -> bool:
        endpoint = f"{self.__server_endpoint}/{model}/unloadable/"

        response = requests.get(endpoint)

        return response.json()

    def get_managed_models(self) -> List[Dict[str, str]]:
        endpoint = f"{self.__server_endpoint}/models/"

        response = requests.get(endpoint)

        return response.json()

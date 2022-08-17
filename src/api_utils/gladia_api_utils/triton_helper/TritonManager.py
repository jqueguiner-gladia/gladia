"""
Notes:
Dans le cas où un modèle triton n'a pas encore été DL (car jamais call et lazy load)
le triton manager ne le trouvera pas et ça sera au TritonClient de le load (ce qui pourrait faire un OOM)
"""

import os
import redis
import tritonclient.http as httptritonclient

from time import time
from logging import getLogger


logger = getLogger(__name__)


class TritonManager:
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
            db=os.getenv("REDIS_DB", 0)
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

    def add_model_to_memory_queue(self, model, first_in_queue=False):
        if f"{self.__TRITON_MODELS_PREFIX}:{model}" not in self.__redis_client.keys(f"{self.__TRITON_MODELS_PREFIX}:*"):
            self.__redis_client.set(
                name=f"{self.__TRITON_MODELS_PREFIX}:{model}",
                value=0 if first_in_queue else time()
            )

    def remove_model_from_memory_queue(self, model):
        if f"{self.__TRITON_MODELS_PREFIX}:{model}" in self.__redis_client.keys(f"{self.__TRITON_MODELS_PREFIX}:*"):
            self.__redis_client.delete(f"{self.__TRITON_MODELS_PREFIX}:{model}")

    def get_memory_queue(self):
        models_in_memory_queue = self.__redis_client.keys("{self.__TRITON_MODELS_PREFIX}:*")

        pipe = self.__redis_client.pipeline()

        for model in models_in_memory_queue:
            pipe.get(model)

        memory_queue = dict(zip(models_in_memory_queue, pipe.execute()))

        return list(memory_queue.keys()).sort(key=lambda model : memory_queue[model])

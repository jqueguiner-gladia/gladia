from email.policy import default
import os
import json
import redis
import uvicorn

from time import time
from fastapi import FastAPI, Query, Body
from typing import Optional, List
from IManager import IManager
from logging import getLogger
from dto import ModelStatus, ModelType
from SingletonMeta import SingletonMeta
from ManagerServerHandler import ManagerServerHandler


class MemoryManager(FastAPI, metaclass=SingletonMeta):

    __logger = getLogger(__name__)

    __redis_client = redis.Redis(
        host=os.getenv("REDIS_HOST", 'localhost'),
        port=os.getenv("REDIS_PORT", 6379),
        db=os.getenv("REDIS_DB", 0),
        decode_responses=True
    )

    __types_manager = {
        ModelType.TRITON: ManagerServerHandler(
            server_url=os.getenv("TRITON_SERVER_URL", "localhost"),
            server_port=os.getenv("TRITON_SERVER_PORT_HTTP", "8080"),
            model_type=ModelType.TRITON,
        ),
        # ModelType.FASTAPI: , # TODO
        # ModelType.LOCAL: , # TODO
    }

    __MAX_MEMORY_USAGE = 0.8

    def __init__(self, title: str = "CustomAPI") -> None:
        super().__init__(title=title)

        @self.post("/{model}/register/")
        async def register_model(model: str, dependencies: Optional[List[str]] = Body(default=None), type: ModelType = Body(...), locked_in_memory: bool = Body(...)):
            model_informations = self.__get_model_informations(
                model_name=model,
                model_type=type,
                default_value_locked_in_memory=locked_in_memory,
                default_value_dependencies=dependencies,
            )

            self.__redis_client.set(
                name=f"{type}:{model}",
                value=json.dumps(model_informations)
            )

            if locked_in_memory:
                for dependency in dependencies:
                    dependency_informations = self.__get_model_informations(
                        model_name=dependency,
                        model_type=type,
                        default_value_locked_in_memory=locked_in_memory,
                        default_value_dependencies=dependencies,
                    )

                    dependency_informations["use_by"] += [model]

                    self.__redis_client.set(
                        name=f"{type}:{dependency}",
                        value=json.dumps(dependency_informations)
                    )

        # TODO: use "locked_in_memory" key in redis to lock the model when needed
        @self.get("/{model}/init/")
        async def init_model_status(model: str, dependencies: Optional[List[str]] = Query(default=None), type: ModelType = Query()):
            manager = self.__types_manager.get(type)(model, dependencies)

            # TODO: implement when we will be able to know how much memory does a model consums
            # if mode.size() < LOW_MEMORY_THRESHOLD:
            #     return True

            import sys
            print("11" * 100, file=sys.stderr)

            if self.__there_is_not_enought_memory_available() or self.__another_model_is_first_in_queue(model):
                memory_queue = self.__get_memory_queue(type)

                if model not in memory_queue:
                    self.__add_model_to_memory_queue(model, manager.__MODEL_TYPE)

                if manager.model_is_loaded(model) and manager.model_can_be_unloaded(model):
                    manager.unload_model(model)

                return {"status": ModelStatus.NOT_READY}

            if self.__model_has_been_loaded(manager, model, dependencies) is False:
                return {"status": ModelStatus.NOT_READY}

            return {"status": ModelStatus.READY}

        @self.get("/{model}/status/")
        async def get_model_status(model: str, dependencies: Optional[List[str]] = Query(default=None), type: ModelType = Query()):

            if self.__another_model_is_first_in_queue(model, model_type=type):
                return {"status": ModelStatus.NOT_READY}

            manager = self.__types_manager.get(type)(model, dependencies)

            if self.__model_has_been_loaded(manager, model, dependencies) is False:
                return {"status": ModelStatus.NOT_READY}

            return {"status": ModelStatus.READY}

        @self.get("/{model}/free/")
        async def free_model(model: str, dependencies: Optional[List[str]] = Query(default=None), type: ModelType = Query()):

            if self.__is_model_a_dependency(model, type) is False:
                self.__update_model_running_status(model, is_running=False)

            for dependency in dependencies:
                self.__remove_dependency_from_model(model=model, dependency=dependency)

                if self.__is_model_a_dependency(dependency, type) is False:
                    self.__update_model_running_status(dependency, is_running=False)

    # TODO: add a security checking if the first model in
    # queue has not been called since x seconds, remove it from queue

    def __create_default_memory_information(self, **kwargs):
        return {
            "running": kwargs.get("default_value_running", False),
            "locked_in_memory": kwargs.get("default_value_locked_in_memory", False),
            "last_call": kwargs.get("default_value_last_call", 0),
            "dependencies": kwargs.get("default_value_dependencies", []),
            "use_by": kwargs.get("default_value_use_by", []),
            "place_in_memory_queue": kwargs.get("default_value_place_in_memory_queue", -1),
        }

    def __get_model_informations(self, model_name, model_type: str, **kwargs):
        if f"{model_type}:{model_name}" not in self.__redis_client.keys(f"{model_type}:*"):
            return self.__create_default_memory_information(**kwargs)

        return json.loads(self.__redis_client.get(
            name=f"{model_type}:{model_name}",
        ))

    def __get_memory_queue(self, model_type: str) -> List[str]:
        models_in_redis = self.__redis_client.keys(f"{model_type}:*")

        if not models_in_redis:
            return []

        pipe = self.__redis_client.pipeline()

        for model in models_in_redis:
            pipe.get(model)

        models_informations = dict(zip(models_in_redis, pipe.execute()))
        models_informations = {model: informations for (model, informations) in models_informations if models_informations[model]["place_in_memory_queue"] >= 0}

        return list(models_informations.keys()).sort(key=lambda model : models_informations[model])

    def __another_model_is_first_in_queue(self, model: str, model_type: str):
        memory_queue = self.__get_memory_queue(model, model_type)

        if len(memory_queue) == 0:
            return False

        return memory_queue[0] != model

    def __add_model_to_memory_queue(self, model: str, model_type: str, first_in_queue: bool = False):
        model_informations = self.__get_model_informations(model, model_type)

        model_informations["place_in_memory_queue"] = 0 if first_in_queue else time()

        self.__redis_client.set(
            name=f"{model_type}:{model}",
            value=json.dumps(model_informations)
        )

    def __remove_model_from_memory_queue(self, model: str, model_type: str):
        model_informations = json.loads(self.__redis_client.get(name=f"{model_type}:{model}"))

        model_informations["place_in_memory_queue"] = -1

        self.__redis_client.set(
            name=f"{model_type}:{model}",
            value=json.dumps(model_informations),
        )

    def __there_is_not_enought_memory_available(self, manager: IManager) -> bool:
        return all([1 - memory_available < self.__MAX_MEMORY_USAGE for memory_available in manager.get_available_memory()])

    def __update_model_running_status(self, model: str, model_type: str, is_running: bool) -> None:
        if not isinstance(is_running, bool):
            raise RuntimeError("is_running parameter must be of type bool")

        model_informations = self.__get_model_informations(model)

        model_informations["running"] = is_running

        if is_running:
            model_informations["last_call"] = time()

        self.__redis_client.set(
            name=f"{model_type}:{model}",
            value=json.dumps(model_informations)
        )

    def __add_dependency_to_model(self, model: str, dependency: str, model_type: str) -> None:
        model_informations = self.__get_model_informations(dependency)

        model_informations["use_by"].append(model)
        model_informations["use_by"] = list(set(model_informations["use_by"]))

        self.__redis_client.set(
            name=f"{model_type}:{dependency}",
            value=json.dumps(model_informations)
        )

        del model_informations

        model_informations = self.__get_model_informations(model)

        model_informations["dependencies"].append(dependency)
        model_informations["dependencies"] = list(set(model_informations["dependencies"]))

        self.__redis_client.set(
            name=f"{model_type}:{model}",
            value=json.dumps(model_informations)
        )


    def __remove_dependency_from_model(self, model: str, dependency: str, model_type: str) -> None:
        model_informations = self.__get_model_informations(dependency)

        model_informations["use_by"].remove(model)

        self.__redis_client.set(
            name=f"{model_type}:{dependency}",
            value=json.dumps(model_informations)
        )

        del model_informations

        model_informations = self.__get_model_informations(model)

        model_informations["dependencies"].remove(dependency)

        self.__redis_client.set(
            name=f"{model_type}:{model}",
            value=json.dumps(model_informations)
        )

    def __is_model_a_dependency(self, model: str, model_type: str) -> None:
        model_informations = self.__get_model_informations(model)

        return len(model_informations["use_by"]) > 0

    def __model_has_been_loaded(self, manager: IManager, model: str, dependencies: List[str]) -> bool:
        if self.__there_is_not_enought_memory_available(manager):
            models_loaded = [model_in_registery["name"] for model_in_registery in self.manager.get_managed_models() if model_in_registery["state"] == Status.LOADED]
            models_that_can_be_unloaded = [model_loaded for model_loaded in models_loaded if manager.model_can_be_unloaded(model)]

            if len(models_that_can_be_unloaded) == 0:
                self.__logger.error("Requesting to unload a model but there is no model to unload, this could result in a infinit loop.")

                return False

            self.manager.unload_model(models_that_can_be_unloaded[0])

            return False

        else:
            manager.load_model(model)
            self.__update_model_running_status(model, model_type=manager.__MODEL_TYPE, is_running=True)

            memory_queue = self.__get_memory_queue(manager.__MODEL_TYPE)

            for dependency in dependencies:
                self.__add_dependency_to_model(model=model, dependency=dependency, model_type=manager.__MODEL_TYPE)
                manager.load_model(dependency) # TODO: check if dependency is not already loaded
                self.__update_model_running_status(dependency, model_type=manager.__MODEL_TYPE, is_running=True)
                self.__remove_model_from_memory_queue(dependency, model_type=manager.__MODEL_TYPE)

            if model in memory_queue:
                self.__remove_model_from_memory_queue(model, model_type=manager.__MODEL_TYPE)

            return True



if __name__ == "__main__":
    memory_manager = MemoryManager()

    uvicorn.run(memory_manager, host=os.getenv("MEMORY_MANAGER_HOST", "0.0.0.0"), port=int(os.getenv("MEMORY_MANAGER_PORT", "8010")))

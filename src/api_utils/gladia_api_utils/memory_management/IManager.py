import abc

from dto import ModelType
from typing import Dict, List

# TODO: IManager should update the model_informations["last_call"] = time() each time the model is called
class IManager(metaclass=abc.ABCMeta):
    __MODEL_TYPE = "UNDEFINED"

    @classmethod
    def __subclasshook__(cls, subclass):
        return (
            hasattr(subclass, '__MODEL_TYPE') and isinstance(subclass.__MODEL_TYPE, ModelType) and
            hasattr(subclass, 'get_available_memory') and callable(subclass.get_available_memory) and 
            hasattr(subclass, 'load_model') and callable(subclass.load_model) and
            hasattr(subclass, 'unload_model') and callable(subclass.unload_model) and
            hasattr(subclass, 'model_can_be_unloaded') and callable(subclass.model_can_be_unloaded) and
            hasattr(subclass, 'get_managed_models') and callable(subclass.get_managed_models)
        )

    @abc.abstractmethod
    def get_available_memory(self) -> List[float]:
        """_summary_

        Raises:
            NotImplementedError: _description_

        Returns:
            bool: _description_
        """

        raise NotImplementedError

    @abc.abstractmethod
    def load_model(self, model: str) -> None:
        """_summary_

        Args:
            model (str): _description_

        Raises:
            NotImplementedError: _description_
        """

        raise NotImplementedError

    @abc.abstractmethod
    def unload_model(self, model: str) -> None:
        """_summary_

        Args:
            model (str): _description_

        Raises:
            NotImplementedError: _description_
        """

        raise NotImplementedError

    @abc.abstractmethod
    def model_can_be_unloaded(self, model: str) -> bool:
        """_summary_

        * Check if the model is running
        * Check if the model is a dependency of another one
        * Check if the model is locked in memory (aka preload one)

        Args:
            model (str): _description_

        Raises:
            NotImplementedError: _description_

        Returns:
            bool: _description_
        """
        raise NotImplementedError

    @abc.abstractmethod
    def get_managed_models(self) -> List[Dict[str, str]]:
        """_summary_

        {
            name: NAME,
            state: STATE # TODO: use Status.THING
        }

        Raises:
            NotImplementedError: _description_

        Returns:
            List[Dict[str, str]]: _description_
        """

        raise NotImplementedError
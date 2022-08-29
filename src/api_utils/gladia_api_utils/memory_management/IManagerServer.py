from fastapi import FastAPI
from IManager import IManager


class IManagerServer(FastAPI, IManager):

    __IRoutes = [
        "/memory/",
        "/{model}/load",
        "/{model}/unload",
        "/{model}/unloadable",
        "/models/",
    ]

    @classmethod
    def __subclasshook__(cls, subclass):

        url_list = [route.path for route in cls.routes]

        return all([route in url_list for route in cls.__IRoutes])

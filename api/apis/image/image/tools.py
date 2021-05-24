import pkgutil

versions = list()


def import_submodules(package, recursive=True):
    """Import all submodules of a module, recursively, including subpackages

    :param package: package (name or actual module)
    :type package: str | module
    :rtype: dict[str, types.ModuleType]
    """
    global versions

    results = {}

    for loader, name, is_pkg in pkgutil.walk_packages(package.__path__):
        full_name = package.__name__ + "." + name
        if recursive and is_pkg:
            versions.append(full_name)
            available_versions.append(full_name.split(".")[-1])

    return results

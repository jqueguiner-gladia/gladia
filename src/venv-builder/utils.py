import os
import yaml
import hashlib
import shutil


def install_packages_in_pipenv_from_file(env_path, env_var_package_file):
    os.system(f"cd {env_path} && pipenv run pip install -r {env_var_package_file}")


def install_packages_in_pipenv_from_string(env_path, package_list_as_string):
    cmd = f"cd {env_path} && pipenv run pip install {package_list_as_string}"
    os.system(cmd)


def install_packages_in_pipenv_from_list(env_path, package_list_as_list):
    # FIXME: it's late I'm tired...
    # fuckit.. weird shit happens as package_list_as_list should be a list
    # but seems to act as a string
    if type(package_list_as_list) is str:
        package_list_as_list = [package_list_as_list]
    package_list_as_string = " ".join(package_list_as_list)
    # handle special cases
    # for git+https://
    # needs -e option in pip install
    if "+" in package_list_as_string:
        for package in package_list_as_list:
            if '+' in package:
                package_list_as_string = install_packages_in_pipenv_from_string(env_path, " -e " + package)
            else:
                package_list_as_string = install_packages_in_pipenv_from_string(env_path, package)
    else:
        install_packages_in_pipenv_from_string(env_path, package_list_as_string)


def build_env_from_modality(path, modality, has_custom_packages, packages_to_install, python_version, envs_base_dict):
    if has_custom_packages:
        print("---------------")
        print(f"Creating env based on {modality} venv")
        print(f"with packages")
        print("---------------")
        boot_full_pipenv_for_modality(path, modality, packages_to_install, envs_base_dict, python_version)

    else:
        print("---------------")
        print(f"Applying simlink to {modality} base venv")
        print(f"without custom packages")
        print("---------------")
        simlink_env_for_modality(path, modality, envs_base_dict)


def boot_pipenv(path, python_version=None):
    if python_version is None:
        python_version = os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']
    os.system(f"cd {path} && echo Y | pipenv --python {python_version}")
    with open(os.path.join(path, ".env"), "a") as f:
        f.write(f"LD_LIBRARY_PATH={os.getenv('LD_LIBRARY_PATH')}\n")


def boot_full_pipenv_for_modality(path, modality, packages_to_install, envs_base_dict, python_version=None):
    boot_pipenv(path, python_version)
    install_packages_in_pipenv_from_file(path, envs_base_dict["common"]['packages_file'])
    install_packages_in_pipenv_from_file(path, envs_base_dict[modality]['packages_file'])
    install_packages_in_pipenv_from_list(path, packages_to_install)


def get_packages(env_yaml):
    has_custom_packages = False
    custom_packages = ''
    try:
        custom_packages = ' '.join(env_yaml['packages'])
        if len(custom_packages) > 0:
            has_custom_packages = True

    except Exception as e:
        print("---------------")
        print("No custom packages found")
        print("---------------")

    return has_custom_packages, custom_packages


def get_env_conf(stream):
    env_yaml = False
    try:
        env_yaml = yaml.safe_load(stream)
        print("---------------")
        print(f"Loaded env.yaml")
        print("---------------")
    except yaml.YAMLError as exc:
        print(exc)
        exit(0)
    return env_yaml


def simlink_if_source_exists(source_path, target_path):
    if not os.path.exists(source_path):
        return

    if os.path.samefile(source_path, target_path):
        return

    if not os.path.islink(target_path):
        if os.path.abspath(source_path) != os.path.abspath(target_path):
            try:
                os.system(f"ln -sf {source_path} {target_path}")
            except Exception as e:
                pass


def simlink_env_for_modality(path, modality, envs_base_dict):
    links = [".venv", "Pipfile"]
    for link in links:
        simlink_if_source_exists(os.path.join(envs_base_dict[modality]['path'], link), os.path.join(path, link))


def simlink_lib_so_files(source_path, target_path):
    source_path = os.path.join(source_path, "lib")
    target_path = os.path.join(target_path, "lib")

    if os.path.samefile(source_path, target_path):
        return

    if os.path.isdir(target_path):
        for file in os.listdir(target_path):
            if file.endswith(".so") and not os.path.islink(file):
                if os.path.exists(os.path.join(source_path, file)) and \
                        compare_files(os.path.join(source_path, file), os.path.join(target_path, file)):
                    simlink_if_source_exists(os.path.join(source_path, file), os.path.join(target_path, file))


def simlink_bin_files(source_path, target_path):
    source_path = os.path.join(source_path, "bin")
    target_path = os.path.join(target_path, "bin")
    
    if os.path.samefile(source_path, target_path):
        return

    if os.path.isdir(target_path):
        for file in os.listdir(target_path):
            if not os.path.islink(file):
                if os.path.exists(os.path.join(source_path, file)):
                    if compare_files(os.path.join(source_path, file), os.path.join(target_path, file)):
                        simlink_if_source_exists(os.path.join(source_path, file), os.path.join(target_path, file))


def simlink_site_packages(source_path, target_path, python_version):
    target_packages_path = os.path.join(target_path, "lib", f"python{python_version}", "site-packages")

    for root, dirs, files in os.walk(target_packages_path, topdown=False):
        for name in files:
            target_file_path = os.path.join(root, name)

            if not os.path.islink(target_file_path):
                # FIXME: some shitty edge case when the file as a space in the name
                if " " not in target_file_path:
                    venv_file_relative_location = target_file_path.split(".venv")[1]
                    source_file_path = source_path + venv_file_relative_location

                    if os.path.exists(source_file_path):
                        if compare_files(source_file_path, target_file_path):
                            simlink_if_source_exists(source_file_path, target_file_path)


def simlink_packages(env_yaml, dirName, pipenv_base):
    # make sure that the custom env python version matches the template
    # to be able to simlink 

    if str(os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']) == str(env_yaml['python']['version']):
        print("---------------")
        print(f"Applying Simlink {pipenv_base} packages to {dirName}")
        print("---------------")

        source_directory = os.path.join(os.environ['PIPENV_VENV_TMP_BASE_PATH'], pipenv_base, ".venv")
        target_directory = os.path.join(dirName, ".venv")

        # simlink lib flat .so
        # /!\ not really sure about this strategy
        # some sheebang are at the top of some and potentialy
        # some lib file
        directories = ["bin", f"lib/python{os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}/site-packages"]

        for directory in directories:
            current_source_directory = os.path.join(source_directory, directory)
            current_target_directory = os.path.join(target_directory, directory)

            for item in os.listdir(current_source_directory):
                clean_dir(os.path.join(current_target_directory, item))
                simlink_if_source_exists(os.path.joint(current_source_directory, item),
                                         os.path.join(current_target_directory, item))

        # simlink lib flat .so
        # /!\ not really sure about this strategy
        # some sheebang are at the top of some and potentialy
        # some lib file
        target_base_dir = os.path.join(target_directory, "lib")
        source_base_dir = os.path.join(source_directory, "lib")
        for item in os.listdir(target_base_dir):
            # exclude folders
            if os.path.isfile(item):
                simlink_if_source_exists(os.path.join(source_base_dir, item), os.path.join(target_base_dir, item))

        print("---------------")
        print(f"Done Simlinking {pipenv_base} env")
        print("---------------")


def md5(filepath):
    hash_md5 = hashlib.md5()
    with open(filepath, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def compare_files(source_path, target_path):
    source_md5 = md5(source_path)
    target_md5 = md5(target_path)

    return source_md5 == target_md5


def mkdir(path):
    os.makedirs(path, exist_ok=True)


def clean_env(path, subdirList, fileList):
    if 'env.yaml' in fileList:
        print("---------------")
        print(f"Cleaning {path}")
        print("---------------")

        dirs_to_clean = [".venv", "Pipfile", "Pipfile.lock", "__pycache__"]
        for dir_to_clean in dirs_to_clean:
            print(f"Cleaning {os.path.join(path, dir_to_clean)}")

            clean_dir(os.path.join(path, dir_to_clean).rstrip("/"))


def clean_useless_prod_packages(path):
    dirs_to_clean = ["wheel*", "pip*", "pip3*"]
    for dir_to_clean in dirs_to_clean:
        os.system(f"cd {path} && rm -rf {dir_to_clean}")


def clean_dir(path):
    os.system(f"rm -rf {path}")


def clean_file(path):
    if os.path.exists(path):
        os.system(f"rm -rf {path}")


def clean_dir_without_parent(path):
    os.system(f"rm -rf {path}")
    os.system(f"mkdir -p {path}")

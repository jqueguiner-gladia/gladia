import os
import yaml
import click
from multiprocessing.pool import ThreadPool as Pool
import multiprocessing
import subprocess
import hashlib
import shutil

# Make the VENV is built and stored nearby the API folder
# https://stackoverflow.com/questions/57919110/how-to-set-pipenv-venv-in-project-on-per-project-basis
# the purpose is to make sure that the API_UTILS catches the existence of the venv when running the inference
os.environ["PIPENV_VENV_IN_PROJECT"] = os.getenv('PIPENV_VENV_IN_PROJECT', 'enabled')
os.environ["PIPENV_VENV_TMP_BASE_PATH"] = os.getenv('PIPENV_VENV_TMP_BASE_PATH', '/tmp/gladia/venv/')
os.environ["PIPENV_VENV_DEFAULT_PY_VERSION"] = os.getenv('PIPENV_VENV_DEFAULT_PY_VERSION', '3.7')

os.environ["PIPENV_VENV_DEFAULT_COMMON_PACKAGES_TXT"] = os.getenv('PIPENV_VENV_DEFAULT_COMMON_PACKAGES_TXT', os.path.join(os.getcwd(), 'default_common_packages.txt'))
os.environ["PIPENV_VENV_DEFAULT_IMAGE_PACKAGES_TXT"] = os.getenv('PIPENV_VENV_DEFAULT_IMAGE_PACKAGES_TXT', os.path.join(os.getcwd(), 'default_image_packages.txt'))
os.environ["PIPENV_VENV_DEFAULT_SOUND_PACKAGES_TXT"] = os.getenv('PIPENV_VENV_DEFAULT_SOUND_PACKAGES_TXT', os.path.join(os.getcwd(), 'default_sound_packages.txt'))
os.environ["PIPENV_VENV_DEFAULT_TEXT_PACKAGES_TXT"] = os.getenv('PIPENV_VENV_DEFAULT_TEXT_PACKAGES_TXT', os.path.join(os.getcwd(), 'default_text_packages.txt'))
os.environ["PIPENV_VENV_DEFAULT_VIDEO_PACKAGES_TXT"] = os.getenv('PIPENV_VENV_DEFAULT_VIDEO_PACKAGES_TXT', os.path.join(os.getcwd(), 'default_video_packages.txt'))


@click.command()
@click.option('-r', '--rootdir', type=str, default='../apis', help="Build env recursively from the provided directory path")
@click.option('-p', '--poolsize', type=int, default=0, help="Parallelness if set to 0 will use all threads")
@click.option('-s', '--simlink', is_flag=True, type=bool, default=False, help="Will simlink gladia-api-utils from the local version of gladia-api-utils")
@click.option('-c', '--compact_mode', is_flag=True, type=bool, default=False, help="Enable compact mode simlinking the default packages")
@click.option('-f', '--force', is_flag=True, type=bool, default=False, help="Force rebuilding venv")
@click.option('-b', '--base', is_flag=True, type=bool, default=False, help="Build the base for custom env")
@click.option('-t', '--trash_cache', is_flag=True, type=bool, default=False, help="Trash the pipenv cache")
@click.option('-l', '--local_venv_trash_cache', is_flag=True, type=bool, default=False, help="Trash the pipenv cache on cust venv")
@click.option('-d', '--teardown_common_env', is_flag=True, type=bool, default=False, help="Tear common venv")
@click.option('-x', '--clean_all_venv', is_flag=True, type=bool, default=False, help="Clean all cust venv")
def main(rootdir, poolsize, simlink, force, base, compact_mode, trash_cache, local_venv_trash_cache, clean_all_venv, teardown_common_env):

    rootdir = os.path.abspath(rootdir)

    global envs_base_dict
    global default_python_version

    default_python_version = str(os.environ["PIPENV_VENV_DEFAULT_PY_VERSION"])
    envs_base_dict = dict()

    envs = ['common', 'image', 'sound', 'text', 'video']
    for env in envs:
        envs_base_dict[env] = {
            'path' : os.path.join(os.environ['PIPENV_VENV_TMP_BASE_PATH'], env),
            'packages_file' : os.environ[f'PIPENV_VENV_DEFAULT_{env.upper()}_PACKAGES_TXT']
        }

    # build template enviroments
    if base:

        envs = ["common", "image", "sound", "text", "video"]
        for env in envs:
            print("---------------")
            print(f"Overriding common base for base {env} venv")
            print("---------------")

            print(f"Preparing {env} env")
            mkdir(envs_base_dict[env]['path'])
            
            print(f"Cleaning {env} env")
            clean_dir_without_parent(envs_base_dict[env]['path'])
            
            print(f"Booting {env} env")
            boot_pipenv(envs_base_dict[env]['path'], default_python_version)
            
            # if not common then stack packages
            if env != "common":
                print("Installing common packages")
                install_packages_in_pipenv_from_file(envs_base_dict[env]['path'], envs_base_dict['common']['packages_file'])
            
            print(f"Installing {env} packages")
            # install packages on top of common
            install_packages_in_pipenv_from_file(envs_base_dict[env]['path'], envs_base_dict[env]['packages_file'])

    # define parallelization strategy
    if poolsize == 0:
        pool = Pool(multiprocessing.cpu_count())
    else:
        pool = Pool(poolsize)
    

    print("---------------")
    print(f"Scanning {rootdir}")
    print("---------------")
    
    # Crawl all endpoints and modalities
    # to build env if necessary
    for dirName, subdirList, fileList in os.walk(rootdir):
        if (".venv" not in dirName) and ("__pycache__" not in dirName):
            if clean_all_venv:
                pool.apply_async(clean_env, (dirName, subdirList, fileList, ))
            else:
                if poolsize == 1:
                    # easier debugging
                    # multi-threading tends to hide errors
                    build_env(dirName, subdirList, fileList, simlink, force, compact_mode, local_venv_trash_cache)
                else:   
                    pool.apply_async(build_env, (dirName, subdirList, fileList, simlink, force, compact_mode, local_venv_trash_cache, ))

    pool.close()
    pool.join()

    if teardown_common_env:
        print("---------------")
        print("cleaning common venv")
        print("---------------")
        clean_dir(os.path.join(os.environ['PIPENV_VENV_TMP_BASE_PATH'], "common"))

    if trash_cache:
        print("---------------")
        print("cleaning general cache")
        print("---------------")
        clean_dir("/root/.cache/*")

def clean_dir(path):
    if os.path.exists(path):
        shutil.rmtree(path)

def clean_file(path):
    if os.path.exists(path):
        os.system(f"rm -rf {path}")


def clean_dir_without_parent(path):
    os.system(f"rm -rf {path}/*")

def mkdir(path):
    os.makedirs(path, exist_ok=True)
    
def boot_pipenv(path, python_version=None):
    if python_version is None:
        python_version = os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']
    print(path)
    os.system(f"cd {path} && echo Y | pipenv --python {python_version}")

def boot_full_pipenv_for_modality(path, modality, packages_to_install, python_version=None):
    boot_pipenv(path, python_version)
    install_packages_in_pipenv_from_file(path, envs_base_dict["common"]['packages_file'])
    install_packages_in_pipenv_from_file(path, envs_base_dict[modality]['packages_file'])
    install_packages_in_pipenv_from_list(path, packages_to_install)

def install_packages_in_pipenv_from_file(env_path, env_var_package_file):
    print(env_var_package_file)
    os.system(f"cd {env_path} && pipenv run pip install -r {env_var_package_file}")

def install_packages_in_pipenv_from_string(env_path, package_list_as_string):

    cmd = f"cd {env_path} && pipenv run pip install {package_list_as_string}"
    print(cmd)
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

def build_env_from_modality(path, modality, has_custom_packages, packages_to_install, python_version):
    if has_custom_packages:
        print("---------------")
        print(f"Creating env based on {modality} venv")
        print(f"with packages")
        print("---------------")
        boot_full_pipenv_for_modality(path, modality, packages_to_install, python_version)

    else:
        print("---------------")
        print(f"Applying simlink to {modality} base venv")
        print(f"without custom packages")
        print("---------------")
        simlink_env_for_modality(path, modality)

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
    if not os.path.islink(target_path):
        os.system(f"[ -d {source_path} ] && ln -sf {source_path} {target_path}")

def simlink_env_for_modality(path, modality):
    links = [".venv", "Pipfile"]
    for link in links:
        simlink_if_source_exists(os.path.join(envs_base_dict[modality]['path'], link), os.path.join(path, link))

def simlink_lib_so_files(source_path, target_path):
    source_path = os.path.join(source_path, "lib")
    target_path = os.path.join(target_path, "lib")
    for file in os.listdir(target_path):
        if file.endswith(".so") and not os.path.islink(file):
            if os.path.exists(os.path.join(source_path, file)):
                if compare_files(os.path.join(source_path, file), os.path.join(target_path, file)):
                    simlink_if_source_exists(os.path.join(source_path, file), os.path.join(target_path, file))

def simlink_bin_files(source_path, target_path):
    source_path = os.path.join(source_path, "bin")
    target_path = os.path.join(target_path, "bin")
    for file in os.listdir(target_path):
        if not os.path.islink(file):
            if os.path.exists(os.path.join(source_path, file)):
                if compare_files(os.path.join(source_path, file), os.path.join(target_path, file)):
                    simlink_if_source_exists(os.path.join(source_path, file), os.path.join(target_path, file))

def simlink_site_packages(source_path, target_path, python_version):
    target_packages_path = os.path.join(target_path, "lib", f"python{python_version}", "site-packages")
    source_packages_path = os.path.join(source_path, "lib", f"python{python_version}", "site-packages")

    for root, dirs, files in os.walk(target_path, topdown = False):
        for name in files:
            target_file_path = os.path.join(root, name)
            
            if not os.path.islink(target_file_path):
                #FIXME: some shitty edge case when the file as a space in the name
                if " " not in target_file_path:
                    venv_file_relative_location= target_file_path.split(".venv")[1]                
                    source_file_path = source_path + venv_file_relative_location

                    if os.path.exists(source_file_path):        
                        if compare_files(source_file_path, target_file_path):
                            simlink_if_source_exists(source_file_path, target_file_path)

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

def clean_env(path, subdirList, fileList):
    if 'env.yaml' in fileList:
        print("---------------")
        print(f"Cleaning {path}")
        print("---------------")
        
        dirs_to_clean = [".venv", "Pipfile", "Pipfile.lock", "__pycache__"]
        for dir_to_clean in dirs_to_clean:
            print(f"Cleaning {os.path.join(path, dir_to_clean)}")

            clean_dir(os.path.join(path, dir_to_clean))
        

def clean_useless_prod_packages(path):
    dirs_to_clean = ["wheel*", "pip*", "pip3*"]
    for dir_to_clean in dirs_to_clean:
        os.system(f"cd {path} && rm -rf {dir_to_clean}")


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
                clean_dir(os.path.join(current_target_directory,item))
                simlink_if_source_exists(os.path.joint(current_source_directory, item), os.path.join(current_target_directory, item))

        # simlink lib flat .so
        # /!\ not really sure about this strategy
        # some sheebang are at the top of some and potentialy
        # some lib file
        target_base_dir = os.path.join(target_directory, "lib")
        source_base_dir = os.path.join(source_directory, "lib")
        for item in os.listdir(target_base_dir):
            #exclude folders
            if os.path.isfile(item):
                # create if source exists only
                # check source existence => [ -d $path_to_source_file ] 
                # and like it => && ln -sf $path_to_source_file $path_to_target_file
                # -f => force override
                simlink_if_source_exists(os.path.join(source_base_dir, item), os.path.join(target_base_dir,item))

        print("---------------")
        print(f"Done Simlinking {pipenv_base} env")
        print("---------------")


def build_env(dirName, subdirList, fileList, simlink, force, compact_mode, local_venv_trash_cache):
    # if env.yaml exist => will trigger a custom env could be
    # the default one from the modality
    if 'env.yaml' in fileList:
        print("---------------")
        print(f"Building {dirName}")
        print("---------------")

        # check the custom env requirements
        with open(os.path.join(dirName, 'env.yaml'), 'r') as stream:
            print("---------------")
            print("Guessing the input modality to apply the right base env")
            
            splitted_path = dirName.split("/")
            input_modality = ""
            for item in splitted_path:
                if item in ["image", "audio", "video", "text"]:
                    input_modality = item
                    break
            
            print(f"Found input modality to be : {input_modality}")
            print("---------------")

            env_yaml = get_env_conf(stream)
            python_version = str(env_yaml['python']['version'])

            # build is a flag
            # that will lead to the build of a 

            # has_custom_packages is a flag to know if
            # you have additionnal package vs the default one from
            # the modality / the env template            
            has_custom_packages, extra_packages_to_install = get_packages(env_yaml)

            custom_env_directory = os.path.join(dirName, '.venv')
            modality_base_env_directory = os.path.join(os.environ['PIPENV_VENV_TMP_BASE_PATH'], input_modality)
            
            # if a custom env seems to be already there
            # depending on the --force arg will trigger a rebuild
            # if no custom env there or forced:
            # if the env has no packages => use the default env of the modality
            # this is checked using the has_custom_packages
            # else build a custom env for the endpoint and try to simlink binaries and libs
            # /!\ linking binaries is complicated as some Sheebangs are happened in general to them
            # binding explicitely the bins to their original location
            
            if os.path.exists(custom_env_directory):
                if force:
                    print("---------------")
                    print("Using Force mode and remove .venv and Pipfile")
                    print("---------------")

                    # when forcing on existing we delete the venv by
                    # removing the .venv and Pipfile
                    # then if has no custom packages we use the 
                    # default modality env
                    # else we build a custom env stacking the 
                    # common + modality + custom
                    # this what build_env_from_modality is doing
                    try:
                        clean_dir(os.path.join(dirName, '.venv'))
                        clean_file(os.path.join(dirName, 'Pipfile'))

                        build_env_from_modality(dirName, input_modality, has_custom_packages, extra_packages_to_install, python_version)

                    except Exception as e: 
                        print("---------------")
                        print("Could not remove .venv and Pipfile")
                        print(str(e))
                        print("---------------")
            
            # if no .venv build it as you have a env.yaml
            else:

                # if custom env needed (because the the env.yaml is there)
                # if no packages => use the default env of the modality
                # we can check the has_custom_packages flag for that purpose
                
                # else build a custom env for the endpoint and try to simlink binaries and libs
                try:
                    print("---------------")
                    print(f"Applying {input_modality} base venv")
                    print("---------------")

                    # if no custom packages => use the default env of the modality
                    # else use build and stack env
                    # this what build_env_from_modality is doing
                    build_env_from_modality(dirName, input_modality, has_custom_packages, extra_packages_to_install, python_version)
               
                except Exception as e: 
                    print("---------------")
                    print("Failed starting creation of custom env")
                    print(str(e))
                    print("---------------")
                    
            
            if compact_mode:
                print("---------------")
                print(f"Running in compact mode")
                print("---------------")

                if default_python_version == python_version:
                    
                    print("---------------")
                    print(f"Applying {input_modality} packages simlinks to custom env")
                    print("---------------")

                    # crawl all files in lib excluding folders
                    # and link them is source exists
                    # starting from common then
                    # continue to modality env
                    # skip else
                    source_path = os.path.join(envs_base_dict[input_modality]['path'], '.venv')
                    target_path = os.path.join(dirName, '.venv')
                    
                    simlink_lib_so_files(source_path, target_path)
                    simlink_bin_files(source_path, target_path)
                    simlink_site_packages(source_path, target_path, python_version)
            
            if simlink:
                print("---------------")
                print(f"Applying Simlink of API UTILS to {dirName}/.venv/lib/python{os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}/site-packages/gladia_api_utils")
                print("---------------")
                
                gladia_utils_source_path = os.path.join('/opt/conda/lib/python' + default_python_version, 'site-packages', 'gladia_api_utils')
                gladia_utils_target_path = os.path.join(dirName, '.venv', 'lib', 'python' + python_version, 'site-packages', 'gladia_api_utils')
                simlink_if_source_exists(gladia_utils_source_path, gladia_utils_target_path)
            else:
                install_packages_in_pipenv_from_string(dirName, 'gladia_api_utils')
            
            if local_venv_trash_cache:
                print("---------------")
                print("cleaning local venv")
                print("---------------")
                
                #clean_useless_prod_packages(dirName)

if __name__ == '__main__':
    main()

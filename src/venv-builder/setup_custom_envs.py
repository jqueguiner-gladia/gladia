import os
import yaml
import click
from multiprocessing.pool import ThreadPool as Pool
import multiprocessing
import subprocess

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
    if base:
        print("---------------")
        print("Building common base venv")
        print("---------------")
        env = "common"
        os.makedirs(f"{os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{env}", exist_ok=True)
        os.system(f"cd {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{env} && echo Y | pipenv --python {os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}")
        env_var_package_file = os.environ[f'PIPENV_VENV_DEFAULT_{env.upper()}_PACKAGES_TXT']
        os.system(f"cd {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{env} && pipenv run pip install -r {env_var_package_file}")


        envs = ["image", "sound", "text", "video"]
        for env in envs:
            print("---------------")
            print(f"Overriding common base for base {env} venv")
            print("---------------")
            os.system(f"rm -rf {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{env}") 
            os.system(f"cp -r {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/common {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{env}")
            f = open(f"{os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{env}", "w")
            f.write(os.environ['PIPENV_VENV_TMP_BASE_PATH'].rstrip('/'))
            f.close()
            env_var_package_file = os.environ[f'PIPENV_VENV_DEFAULT_{env.upper()}_PACKAGES_TXT']
            os.system(f"cd {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{env} && pipenv run pip install -r {env_var_package_file}")

    if poolsize == 0:
        pool = Pool(multiprocessing.cpu_count())
    else:
        pool = Pool(poolsize)
    
    print("---------------")
    print(f"Scanning {rootdir}")
    print("---------------")
    
    for dirName, subdirList, fileList in os.walk(rootdir):
        if (".venv" not in dirName) and ("__pycache__" not in dirName):
            if clean_all_venv:
                pool.apply_async(clean_env, (dirName, subdirList, fileList, ))
            else:
                pool.apply_async(build_env, (dirName, subdirList, fileList, simlink, force, compact_mode, local_venv_trash_cache, ))

    pool.close()
    pool.join()

    if teardown_common_env:
        print("---------------")
        print("cleaning common venv")
        print("---------------")
        os.system(f"rm -rf {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/common")

    if trash_cache:
        print("---------------")
        print("cleaning general cache")
        print("---------------")
        os.system("rm -rf /root/.cache/*")

def clean_env(dirName, subdirList, fileList):
    if 'env.yaml' in fileList:
        print("---------------")
        print(f"Cleaning {dirName}")
        print("---------------")
        
        os.system(f"cd {dirName} && rm -rf .venv Pipfile __pycache__")

# def simlink_package(env_yaml, dirName, pipenv_base):
#     if str(os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']) == str(env_yaml['python']['version']):
#         print("---------------")
#         print(f"Applying Simlink {pipenv_base} packages to {dirName}.venv/lib/python{os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}/site-packages/")
#         print("---------------")
        
#         # diff -qr /tmp/gladia/cust_venv_base /tmp/gladia-empty/
#         # output keep :
#         # .venv/bin except:
#         # .venv/bin/pip*
#         # .venv/bin/wheel*
#         # .venv/bin/activate*
#         # .venv/bin/easy_install*
#         # 
#         # .venv/lib recursive except:
#         # no-exceptions so far

#         cmd_diff = [
#             "diff", 
#             "-qr",
#             f"{os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{pipenv_base}/.venv/",
#             f"{os.getcwd()}/{dirName}/.venv/",
#             '| grep "Only in"',
#             '| grep -v "share"',
#             '| grep -v "licensing"',
#             f'| grep -v "{os.getcwd()}"',

#             ]
#         cmd_diff = ' '.join(cmd_diff)
        
#         print("============")
#         print(cmd_diff)
#         print("============")
#         diffs = subprocess.run(cmd_diff, stdout=subprocess.PIPE, shell=True).stdout.decode('utf-8')

#         diffs = diffs.replace("Only in ","").replac.split("\n")

#         for diff_packages in diffs:
#             if len(diff_packages.split(": ")) == 2:
#                 try:
#                     source_dir, filename = diff_packages.split(": ")

#                     target_dir = source_dir.replace(f"{os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{pipenv_base}/.venv/", f"{os.getcwd()}/{dirName}/.venv/")             
                    
#                     print(f"ln -sf {source_dir}/{filename} {target_dir}/{filename}")
                    
#                     os.system(f"ln -sf {source_dir}/{filename} {target_dir}/{filename}")

#                 except Exception as e: 
#                     print("---------------")
#                     print(f"Failed Simlinking {pipenv_base} env")
#                     print("---------------")
#                     print(str(e))
#         print("---------------")
#         print(f"Done Simlinking {pipenv_base} env")
#         print("---------------")

def simlink_packages(env_yaml, dirName, pipenv_base):
    if str(os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']) == str(env_yaml['python']['version']):
        print("---------------")
        print(f"Applying Simlink {pipenv_base} packages to {dirName}")
        print("---------------")
        
        source_directory = os.path.join(os.environ['PIPENV_VENV_TMP_BASE_PATH'], pipenv_base, ".venv")
        target_directory = os.path.join(dirName, ".venv")
        directories = ["bin", "lib/python3.7/site-packages"]
        
        for directory in directories:
            current_source_directory = os.path.join(source_directory, directory)
            current_target_directory = os.path.join(target_directory, directory)
            for item in os.listdir(current_source_directory):
                os.system(f"rm -rf {current_target_directory}/{item}")
                cmd = f"ln -sf {current_source_directory}/{item} {current_target_directory}/{item}"
                #print(cmd)
                os.system(cmd)

        # simlink lib flat .so
        target_base_dir = os.path.join(target_directory, "lib")
        source_base_dir = os.path.join(source_directory, "lib")
        for item in os.listdir(target_base_dir):
            #exclude folders
            if os.path.isfile(item):
                # create if source exists only
                # check source existence => [ -d $path_to_source_file ] 
                # and like it => && ln -sf $path_to_source_file $path_to_target_file
                # -f => force override
                os.system(f"[ -d {source_base_dir}/{item} ] && ln -sf {source_base_dir}/{item} {target_base_dir}/{item})")

        
        print("---------------")
        print(f"Done Simlinking {pipenv_base} env")
        print("---------------")

def build_env(dirName, subdirList, fileList, simlink, force, compact_mode, local_venv_trash_cache):
    if 'env.yaml' in fileList:
        print("---------------")
        print(f"Building {dirName}")
        print("---------------")

        with open(os.path.join(dirName, 'env.yaml'), 'r') as stream:
            print("---------------")
            print("Guessing the input modality to apply the right base env")
            input_modality = dirName.split("/")[2]
            print(f"Found input modality to be : {input_modality}")
            print("---------------")

            try:
                env_yaml = yaml.safe_load(stream)
                print("---------------")
                print(f"Loaded env.yaml in {dirName}")
                print("---------------")
            except yaml.YAMLError as exc:
                print(exc)

            build = True
            if os.path.exists(os.path.join(dirName, '.venv')):
                if force == True:
                    print("---------------")
                    print("Using Force mode and remove .venv and Pipfile")
                    print("---------------")
                    try:
                        os.system(f"cd {dirName} && rm -rf .venv Pipfile")
                        os.system(f"cp -r {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{input_modality}/.venv {dirName}/.venv")
                    except:
                        print("---------------")
                        print("Could not remove .venv and Pipfile")
                        print("---------------")
                        build = False
                else:
                    build = False
            else:
                try:
                    print("---------------")
                    print(f"Applying {input_modality} base venv")
                    os.system(f"cp -r {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{input_modality}/.venv {dirName}/.venv")
                    print("---------------")
               
                except Exception as e: 
                    print("---------------")
                    print("Failed starting creation of custom env")
                    print("---------------")
                    print(str(e))
            try:
                packages_to_install = ' '.join(env_yaml['packages'])
            except:
                packages_to_install = ''

            if compact_mode:
                print("---------------")
                print(f"Running in compact mode")
                print("---------------")

                if str(os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']) == str(env_yaml['python']['version']):
                    #print("---------------")
                    #print("Applying Common packages simlinks")
                    #print("---------------")
                    #simlink_packages(env_yaml=env_yaml, dirName=dirName, pipenv_base="common")
                    print("---------------")
                    print(f"Applying {input_modality} packages simlinks")
                    print("---------------")
                    if packages_to_install.strip() == '':
                        print("---------------")
                        print(f"Empty custom packages applying full simlink to venv")
                        print("---------------")
                        os.system(f"rm -rf {dirName}/.venv")
                        os.system(f"ln -sf {os.environ['PIPENV_VENV_TMP_BASE_PATH']}/{input_modality}/.venv {dirName}/.venv")
                        build = False
                    else:                    
                        simlink_packages(env_yaml=env_yaml, dirName=dirName, pipenv_base=input_modality)


                else:
                    print("---------------")
                    print(f"Python version of app do not matches with default version running in non compact mode")
                    print("---------------")
                    with open(os.environ["PIPENV_VENV_DEFAULT_PACKAGES_TXT"]) as f:
                        packages_to_install += " ".join(line.strip() for line in f)
                
            if simlink:
                print("---------------")
                print(f"Applying Simlink of API UTILS to {dirName}/.venv/lib/python{os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}/site-packages/gladia_api_utils")
                print("---------------")
                
                
                os.system(f"rm -rf {dirName}/.venv/lib/python{os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}/site-packages/gladia_api_utils")
                os.system(f"ln -s /opt/conda/lib/python{os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}/site-packages/gladia_api_utils {dirName}/.venv/lib/python{os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}/site-packages/gladia_api_utils")
            else:
                packages_to_install += ' gladia-api-utils'

            if build:
                print("---------------")
                print(f"Overriding {input_modality} base env with custom packages")
                print(f"for {dirName}")
                print("---------------")

                os.system(f"cd {dirName} && pipenv run pip install {packages_to_install}")
            
            if local_venv_trash_cache:
                print("---------------")
                print("cleaning local venv")
                print("---------------")
                
                os.system(f"cd {dirName}/.venv/lib/python{os.environ['PIPENV_VENV_DEFAULT_PY_VERSION']}/site-packages && rm -rf wheel* pip* pip3*")

if __name__ == '__main__':
    main()

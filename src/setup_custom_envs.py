#!/usr/bin/env python3
import os
import yaml
import click
from multiprocessing.pool import ThreadPool as Pool
import multiprocessing

@click.command()
@click.option('-r', '--rootdir', type=str, default='apis', help="Build env recursively from the provided directory path")
@click.option('-p', '--poolsize', type=int, default=0, help="Parallelness if set to 0 will use all threads")
def main(rootdir, poolsize):
    
    if poolsize == 0:
        pool = Pool(multiprocessing.cpu_count())
    else:
        pool = Pool(poolsize)

    for dirName, subdirList, fileList in os.walk(rootdir):
        pool.apply_async(build_env, (dirName, subdirList, fileList,))

    pool.close()
    pool.join()

def build_env(dirName, subdirList, fileList):
    if 'env.yaml' in fileList:
        print(f"Found env.yaml in {dirName}")
        with open(os.path.join(dirName, 'env.yaml'), 'r') as stream:
            try:
                env_yaml = yaml.safe_load(stream)
                print(f"Loaded env.yaml in {dirName}")
            except yaml.YAMLError as exc:
                print(exc)
            
            print(env_yaml)

            try:
                os.system(f"cd {dirName} && rm -rf .env Pipfile")
            except:
                print("Could not remove .env and Pipfile")
            
            packages_to_install = ' '.join(env_yaml['packages']) + ' gladia-api-utils'
            os.system(f"cd {dirName} && echo Y | pipenv --python {env_yaml['python']['version']}")
            os.system(f"cd {dirName} && pipenv run pip install {packages_to_install}")


if __name__ == '__main__':
    main()

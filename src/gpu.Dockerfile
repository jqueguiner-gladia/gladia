#https://www.docker.com/blog/advanced-dockerfiles-faster-builds-and-smaller-images-using-buildkit-and-multistage-builds/
ARG GLADIA_DOCKER_BASE=docker.io/gladiaio/gladia-base:latest

FROM $GLADIA_DOCKER_BASE


# add build options to setup_custom_envs
# can be -f to force rebuild of env if already exist
# -p 1 is set by default for stability purposes
# python3 setup_custom_envs.py --help
#
#Usage: setup_custom_envs.py [OPTIONS]
#
#Options:
#  -r, --rootdir TEXT            Build env recursively from the provided
#                                directory path
#  -p, --poolsize INTEGER        Parallelness if set to 0 will use all threads
#  -s, --simlink                 Will simlink gladia-api-utils from the local
#                                version of gladia-api-utils
#  -c, --compact_mode            Enable compact mode simlinking the default
#                                packages
#  -f, --force                   Force rebuilding venv
#  -b, --base                    Build the base for custom env
#  -t, --trash_cache             Trash the pipenv cache
#  -l, --local_venv_trash_cache  Trash the pipenv cache on cust venv
#  -d, --teardown_common_env     Tear common venv
#  -B, --build_all_env           Build all cust venv
#  -x, --clean_all_venv          Clean all cust venv
#  --help                        Show this message and exit.

ARG SETUP_CUSTOM_ENV_BUILD_MODE="--local_venv_trash_cache --force --simlink --compact_mode --poolsize 1 --base --build_all_env" \
    SKIP_CUSTOM_ENV_BUILD="false" \
    SKIP_ROOT_CACHE_CLEANING="false" \
    SKIP_PIP_CACHE_CLEANING="false" \
    SKIP_YARN_CACHE_CLEANING="false" \
    SKIP_NPM_CACHE_CLEANING="false" \
    SKIP_TMPFILES_CACHE_CLEANING="false" \
    SKIP_NTLK_DL="false" \
    GLADIA_API_UTILS_BRANCH="main"

## TOKENIZERS_PARALLELISM is not used anywhere. can be removed no ?
## why defining the place of the cache for transformers pytorch pytorch pretrained ?
## it should be done by default in the place : # talking points
ENV PIPENV_VENV_IN_PROJECT="enabled" \
    TOKENIZERS_PARALLELISM="true" \
    TRANSFORMERS_CACHE="/tmp/gladia/models/transformers" \
    PYTORCH_TRANSFORMERS_CACHE="/tmp/gladia/models/pytorch_transformers" \
    PYTORCH_PRETRAINED_BERT_CACHE="/tmp/gladia/models/pytorch_pretrained_bert" \
    NLTK_DATA="/tmp/gladia/nltk" \
    LC_ALL="C.UTF-8" \
    LANG="C.UTF-8"

# to be remove later
# hack because JL fucked up the base image
RUN rm -rf /app

COPY . /app

## why uninstalling and re installing here
## all this should be put in requirements.txt ?
## why putting it here ?
RUN for package in $(cat /app/requirements.txt); do echo "================="; echo "installing ${package}"; echo "================="; pip3 install $package; done && \
    pip3 uninstall -y gladia-api-utils \ 
    pip3 uninstall -y botocore transformers && \
    pip3 install botocore transformers && \
    pip3 uninstall -y pyarrow && \
    pip3 install pyarrow>=5.0.0 && \
    sh /app/clean-layer.sh && \
    rm /app/clean-layer.sh && \
    pip3 install git+https://github.com/gladiaio/gladia-api-utils.git\@$GLADIA_API_UTILS_BRANCH

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then cd /app/venv-builder && python3 setup_custom_envs.py -x -r /app/apis/ && python3 setup_custom_envs.py $SETUP_CUSTOM_ENV_BUILD_MODE; fi 
## NLTK lib is in default_text_packages... why adding it here ?
RUN if [ "$SKIP_NTLK_DL" = "false" ]; then python3 -c 'import nltk ;nltk.download("omw-1.4")'; fi 
RUN if [ "$SKIP_ROOT_CACHE_CLEANING" = "false" ]; then [ -d "/root/.cache/" ] && rm -rf "/root/.cache/*"; fi
RUN if [ "$SKIP_PIP_CACHE_CLEANING" = "false" ]; then rm -rf "/tmp/pip*"; fi
RUN if [ "$SKIP_YARN_CACHE_CLEANING" = "false" ]; then rm -rf "/tmp/yarn*"; fi
RUN if [ "$SKIP_NPM_CACHE_CLEANING" = "false" ]; then rm -rf "/tmp/npm*"; fi 
RUN if [ "$SKIP_TMPFILES_CACHE_CLEANING" = "false" ]; then rm -rf "/tmp/tmp*"; fi 
RUN apt-get clean 
RUN apt-get autoremove --purge 
RUN conda clean --all 

WORKDIR /app
EXPOSE 80

CMD ["sh", "-c", "echo $PWD && sh run_server_prod.sh"]

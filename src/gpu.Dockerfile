ARG GLADIA_DOCKER_BASE=docker.io/gladiaio/gladia-base:latest

FROM $GLADIA_DOCKER_BASE as build

ADD clean-layer.sh  /tmp/clean-layer.sh

COPY requirements.txt /tmp/gladia-requirements.txt

RUN for package in $(cat /tmp/gladia-requirements.txt); do echo "================="; echo "installing ${package}"; echo "================="; pip3 install $package; done

RUN pip uninstall -y gladia-api-utils

ARG GLADIA_API_UTILS_BRANCH=main
RUN pip3 install git+https://github.com/gladiaio/gladia-api-utils.git\@$GLADIA_API_UTILS_BRANCH

RUN pip3 uninstall -y botocore transformers
RUN pip3 install botocore transformers
RUN pip3 uninstall -y pyarrow
RUN pip3 install pyarrow>=5.0.0
RUN rm /tmp/clean-layer.sh

ENV PIPENV_VENV_IN_PROJECT="enabled"

ENV TOKENIZERS_PARALLELISM="true"

ENV TRANSFORMERS_CACHE="/tmp/gladia/models/transformers"
ENV PYTORCH_TRANSFORMERS_CACHE="/tmp/gladia/models/pytorch_transformers"
ENV PYTORCH_PRETRAINED_BERT_CACHE="/tmp/gladia/models/pytorch_pretrained_bert"
ENV NLTK_DATA="/tmp/gladia/nltk"


COPY . /app
WORKDIR /app

# add build options to setup_custom_envs
# can be -f to force rebuild of env if already exist
# -p 1 is set by default for stability purposes
# python3 setup_custom_envs.py --help
# Usage: setup_custom_envs.py [OPTIONS]
# 
# Options:
#   -r, --rootdir TEXT            Build env recursively from the provided
#                                  directory path
#   -p, --poolsize INTEGER        Parallelness if set to 0 will use all threads
#   -s, --simlink                 Will simlink gladia-api-utils from the local
#                                 version of gladia-api-utils
#   -c, --compact_mode            Enable compact mode simlinking the default
#                                 packages
#   -f, --force                   Force rebuilding venv
#   -b, --base                    Build the base for custom env
#   -t, --trash_cache             Trash the pipenv cache
#   -l, --local_venv_trash_cache  Trash the pipenv cache on cust venv
#   --help                        Show this message and exit.

ARG SETUP_CUSTOM_ENV_BUILD_MODE="--local_venv_trash_cache --force --simlink --compact_mode --poolsize 0 --base"
ARG SKIP_CUSTOM_ENV_BUILD="false"
ARG SKIP_CACHE_CLEANING="false"
ARG SKIP_NTLK_DL="false"

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then cd venv-builder && python3 setup_custom_envs.py $SETUP_CUSTOM_ENV_BUILD_MODE; fi

# import omw
RUN if [ "$SKIP_NTLK_DL" = "false" ]; then python3 -c 'import nltk ;nltk.download("omw-1.4")'; fi

RUN if [ "$SKIP_CACHE_CLEANING" = "false" ]; then rm -rf "/root/.cache/*"; fi

EXPOSE 80

CMD ["sh", "-c", "echo $PWD && sh run_server_prod.sh"]

# squash docker image
FROM scratch as prod

COPY --from=build / /

EXPOSE 80

CMD ["sh", "-c", "echo $PWD && sh run_server_prod.sh"]

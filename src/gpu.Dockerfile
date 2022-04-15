#https://www.docker.com/blog/advanced-dockerfiles-faster-builds-and-smaller-images-using-buildkit-and-multistage-builds/
ARG GLADIA_DOCKER_BASE=docker.io/gladiaio/gladia-base:latest

FROM $GLADIA_DOCKER_BASE as buildbase

ENV PIPENV_VENV_IN_PROJECT="enabled"
ENV TOKENIZERS_PARALLELISM="true"
ENV TRANSFORMERS_CACHE="/tmp/gladia/models/transformers"
ENV PYTORCH_TRANSFORMERS_CACHE="/tmp/gladia/models/pytorch_transformers"
ENV PYTORCH_PRETRAINED_BERT_CACHE="/tmp/gladia/models/pytorch_pretrained_bert"
ENV NLTK_DATA="/tmp/gladia/nltk"

ENV LC_ALL="C.UTF-8"
ENV LANG="C.UTF-8"

ARG GLADIA_API_UTILS_BRANCH="main"
ADD clean-layer.sh  /tmp/clean-layer.sh

COPY requirements.txt /tmp/gladia-requirements.txt

RUN for package in $(cat /tmp/gladia-requirements.txt); do echo "================="; echo "installing ${package}"; echo "================="; pip3 install $package; done
RUN pip uninstall -y gladia-api-utils
RUN pip3 install git+https://github.com/gladiaio/gladia-api-utils.git\@$GLADIA_API_UTILS_BRANCH
RUN pip3 uninstall -y botocore transformers
RUN pip3 install botocore transformers
RUN pip3 uninstall -y pyarrow
RUN pip3 install pyarrow>=5.0.0

RUN rm /tmp/clean-layer.sh


# used for dev 
# then just run
# docker run -it -v $PWD:/app -v /tmp/gladia/:/tmp/gladia gladiadev
FROM buildbase as builddev
WORKDIR /app
EXPOSE 80
ENTRYPOINT /bin/bash


FROM builddev as buildpreprod

COPY --from=builddev / /

ARG SETUP_CUSTOM_ENV_BUILD_MODE="--local_venv_trash_cache --force --simlink --compact_mode --poolsize 0 --base"
ARG SKIP_CUSTOM_ENV_BUILD="false"
ARG SKIP_ROOT_CACHE_CLEANING="false"
ARG SKIP_PIP_CACHE_CLEANING="false"
ARG SKIP_YARN_CACHE_CLEANING="false"
ARG SKIP_NPM_CACHE_CLEANING="false"
ARG SKIP_TMPFILES_CACHE_CLEANING="false"
ARG SKIP_NTLK_DL="false"
ARG GLADIA_API_UTILS_BRANCH="main"

ENV PIPENV_VENV_IN_PROJECT="enabled"
ENV TOKENIZERS_PARALLELISM="true"
ENV TRANSFORMERS_CACHE="/tmp/gladia/models/transformers"
ENV PYTORCH_TRANSFORMERS_CACHE="/tmp/gladia/models/pytorch_transformers"
ENV PYTORCH_PRETRAINED_BERT_CACHE="/tmp/gladia/models/pytorch_pretrained_bert"
ENV NLTK_DATA="/tmp/gladia/nltk"

ENV LC_ALL="C.UTF-8"
ENV LANG="C.UTF-8"

COPY . /app

WORKDIR /app
EXPOSE 80

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

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then cd venv-builder && python3 setup_custom_envs.py $SETUP_CUSTOM_ENV_BUILD_MODE; fi

# import omw
RUN if [ "$SKIP_NTLK_DL" = "false" ]; then python3 -c 'import nltk ;nltk.download("omw-1.4")'; fi

RUN if [ "$SKIP_ROOT_CACHE_CLEANING" = "false" ]; then rm -rf "/root/.cache/*"; fi

RUN if [ "$SKIP_PIP_CACHE_CLEANING" = "false" ]; then rm -rf "/tmp/pip*"; fi

RUN if [ "$SKIP_YARN_CACHE_CLEANING" = "false" ]; then rm -rf "/tmp/yarn*"; fi

RUN if [ "$SKIP_NPM_CACHE_CLEANING" = "false" ]; then rm -rf "/tmp/npm*"; fi

RUN if [ "$SKIP_TMPFILES_CACHE_CLEANING" = "false" ]; then rm -rf "/tmp/tmp*"; fi

CMD ["sh", "-c", "echo $PWD && sh run_server_prod.sh"]


# squash docker image
FROM scratch as buildprod

COPY --from=buildpreprod / /

ENV PIPENV_VENV_IN_PROJECT="enabled"
ENV TOKENIZERS_PARALLELISM="true"
ENV TRANSFORMERS_CACHE="/tmp/gladia/models/transformers"
ENV PYTORCH_TRANSFORMERS_CACHE="/tmp/gladia/models/pytorch_transformers"
ENV PYTORCH_PRETRAINED_BERT_CACHE="/tmp/gladia/models/pytorch_pretrained_bert"
ENV NLTK_DATA="/tmp/gladia/nltk"

ENV LC_ALL="C.UTF-8"
ENV LANG="C.UTF-8"

WORKDIR /app
EXPOSE 80

CMD ["sh", "-c", "echo $PWD && sh run_server_prod.sh"]

ARG GLADIA_DOCKER_BASE=docker.io/gladiaio/gladia-base:latest

FROM $GLADIA_DOCKER_BASE

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
ENV PYTORCH_PRETRAINED_BERT_CACHE=="/tmp/gladia/models/pytorch_pretrained_bert"

COPY . /app
WORKDIR /app

RUN python3 setup_custom_envs.py

# import omw
RUN python3 -c 'import nltk ;nltk.download("omw-1.4")'

EXPOSE 80

CMD ["sh", "-c", "echo $PWD && sh run_server_prod.sh"]

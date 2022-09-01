ARG GLADIA_BASE_IMAGE=gladiaio/gladia-base:latest
FROM $GLADIA_BASE_IMAGE

ARG SKIP_CUSTOM_ENV_BUILD="false"
ARG SKIP_ROOT_CACHE_CLEANING="false"
ARG SKIP_PIP_CACHE_CLEANING="false"
ARG SKIP_YARN_CACHE_CLEANING="false"
ARG SKIP_NPM_CACHE_CLEANING="false"
ARG SKIP_TMPFILES_CACHE_CLEANING="false"
ARG GLADIA_TMP_PATH="/tmp/gladia/"
ARG PATH_TO_GLADIA_SRC="/app/"
ARG PATH_TO_APIS_SRC=$PATH_TO_GLADIA_SRC/apis/
ARG DOCKER_USER=root
ARG DOCKER_GROUP=root
ARG API_SERVER_PORT_HTTP=8080
ARG MAMBA_ALWAYS_SOFTLINK="true"
ARG CLEAN_LAYER_SCRIPT=$PATH_TO_GLADIA_SRC/tools/docker/clean-layer.sh
ARG VENV_BUILDER_PATH=$PATH_TO_GLADIA_SRC/tools/venv-builder/

# TF_CPP_MIN_LOG_LEVEL=2 
# https://stackoverflow.com/questions/35911252/disable-tensorflow-debugging-information
# 0 = all messages are logged (default behavior)
# 1 = INFO messages are notcd  printed
# 2 = INFO and WARNING messages are not printed
# 3 = INFO, WARNING, and ERROR messages are not printed
ENV GLADIA_TMP_PATH=$GLADIA_TMP_PATH \
    PATH_TO_GLADIA_SRC=$PATH_TO_GLADIA_SRC \
    API_SERVER_PORT_HTTP=$API_SERVER_PORT_HTTP \
    VENV_BUILDER_PATH=$VENV_BUILDER_PATH \
    NLTK_DATA=$GLADIA_TMP_PATH/nltk_data \
    TF_CPP_MIN_LOG_LEVEL=2 \
    CUDA_DEVICE_ORDER=PCI_BUS_ID

RUN mkdir -p $GLADIA_TMP_PATH \
    mkdir -p $NLTK_DATA

COPY . $PATH_TO_GLADIA_SRC

# Automatically activate micromaba for every bash shell
RUN mv $PATH_TO_GLADIA_SRC/tools/docker/_activate_current_env.sh /usr/local/bin/ && \
    echo "source /usr/local/bin/_activate_current_env.sh" >> ~/.bashrc && \
    echo "source /usr/local/bin/_activate_current_env.sh" >> /etc/skel/.bashrc && \
    echo "micromamba activate server" >> ~/.bashrc

WORKDIR $PATH_TO_GLADIA_SRC

RUN micromamba create -f env.yaml && \
    $PATH_TO_GLADIA_SRC/tools/docker/clean-layer.sh

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server --cwd $VENV_BUILDER_PATH /bin/bash -c "python3 create_custom_envs.py --modality '.*/apis/text/[a-zA-Z ]+/[a-rA-R].*'"; \
    fi  && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server --cwd $VENV_BUILDER_PATH /bin/bash -c "python3 create_custom_envs.py --modality '.*/apis/text/[a-zA-Z ]+/[s-zS-Z].*'"; \
    fi  && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server --cwd $VENV_BUILDER_PATH /bin/bash -c "python3 create_custom_envs.py --modality '.*/apis/video/.*'"; \
    fi  && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server --cwd $VENV_BUILDER_PATH /bin/bash -c "python3 create_custom_envs.py --modality '.*/apis/image/[a-zA-Z ]+/[a-hA-H].*'"; \
    fi && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server --cwd $VENV_BUILDER_PATH /bin/bash -c "python3 create_custom_envs.py --modality '.*/apis/image/[a-zA-Z ]+/[i-zI-Z].*'"; \
    fi && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server --cwd $VENV_BUILDER_PATH /bin/bash -c "python3 create_custom_envs.py --modality '.*/apis/audio/.*'"; \ 
    fi && \
    $CLEAN_LAYER_SCRIPT

ENV LD_PRELOAD="/opt/tritonserver/backends/pytorch/libmkl_rt.so" \
    LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$MAMBA_ROOT_PREFIX/envs/server/lib/"

RUN echo "== ADJUSTING binaries ==" && \ 
    mv /usr/bin/python3 /usr/bin/python38 && \
    ln -sf /usr/bin/python /usr/bin/python3 && \
    echo "== ADJUSTING entrypoint ==" && \ 
    mv $PATH_TO_GLADIA_SRC/tools/docker/entrypoint.sh /opt/nvidia/nvidia_entrypoint.sh && \
    echo "== ADJUSTING path rights ==" && \ 
    chown -R $DOCKER_USER:$DOCKER_GROUP $PATH_TO_GLADIA_SRC && \
    chown -R $DOCKER_USER:$DOCKER_GROUP $GLADIA_TMP_PATH && \
    echo "== FIXING libcurl references ==" && \ 
    rm $MAMBA_ROOT_PREFIX/envs/server/lib/libcurl.so.4 && \
    ln -s /usr/lib/x86_64-linux-gnu/libcurl.so.4.6.0 $MAMBA_ROOT_PREFIX/envs/server/lib/libcurl.so.4 && \
    $CLEAN_LAYER_SCRIPT

EXPOSE $API_SERVER_PORT_HTTP

ENTRYPOINT ["micromamba", "run", "-n", "server"]

CMD ["/app/run_server.sh"]


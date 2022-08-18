FROM docker.gladia.io/gladia-base

ARG SKIP_CUSTOM_ENV_BUILD="false"
ARG SKIP_ROOT_CACHE_CLEANING="false"
ARG SKIP_PIP_CACHE_CLEANING="false"
ARG SKIP_YARN_CACHE_CLEANING="false"
ARG SKIP_NPM_CACHE_CLEANING="false"
ARG SKIP_TMPFILES_CACHE_CLEANING="false"
ARG GLADIA_TMP_PATH="/tmp/gladia/"
ARG PATH_TO_GLADIA_SRC="/app/"
ARG DOCKER_USER=root
ARG DOCKER_GROUP=root
ARG API_SERVER_PORT_HTTP=8080
ARG MAMBA_ALWAYS_SOFTLINK="true"
ARG CLEAN_LAYER_SCRIPT=$PATH_TO_GLADIA_SRC/tools/docker/clean-layer.sh
ARG VENV_BUILDER_PATH=$PATH_TO_GLADIA_SRC/tools/venv-builder/

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
        micromamba run -n server /bin/bash -c "cd $VENV_BUILDER_PATH && python3 create_custom_envs.py --modality '.*/apis/text/[a-zA-Z ]+/[a-rA-R].*'"; \
    fi  && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server /bin/bash -c "cd $VENV_BUILDER_PATH && python3 create_custom_envs.py --modality '.*/apis/text/[a-zA-Z ]+/[s-zS-Z].*'"; \
    fi  && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server /bin/bash -c "cd $VENV_BUILDER_PATH && python3 create_custom_envs.py --modality '.*/apis/video/.*'"; \
    fi  && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server /bin/bash -c "cd $VENV_BUILDER_PATH && python3 create_custom_envs.py --modality '.*/apis/image/[a-zA-Z ]+/[a-hA-H].*'"; \
    fi && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server /bin/bash -c "cd $VENV_BUILDER_PATH && python3 create_custom_envs.py --modality '.*/apis/image/[a-zA-Z ]+/[i-zI-Z].*'"; \
    fi && \
    $CLEAN_LAYER_SCRIPT

RUN if [ "$SKIP_CUSTOM_ENV_BUILD" = "false" ]; then \
        micromamba run -n server /bin/bash -c "cd $VENV_BUILDER_PATH && python3 create_custom_envs.py --modality '.*/apis/audio/.*'"; \ 
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


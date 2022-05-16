python warm_up.py &&
docker run \                             
  --gpus=1 \
  --ipc=host --rm \
  --shm-size=1g \
  --ulimit memlock=-1 \
  --ulimit stack=67108864 \
  -p ${TRITON_SERVER_PORT}:8000 -p 8001:8001 -p 8002:8002 \
  -v $PWD/triton:/models \
  nvcr.io/nvidia/tritonserver:22.04-py3 \
  tritonserver \
  --model-repository=${TRITON_MODELS_PATH}
  --exit-on-error=false \
  --model-control-mode=explicit \
  --repository-poll-secs 10 \
    --allow-metrics=false \
& uvicorn main:app --host 0.0.0.0 --port 80 --workers 8
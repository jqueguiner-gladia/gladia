API_SERVER_PORT_HTTP="${API_SERVER_PORT_HTTP:-8080}"
API_SERVER_WORKERS="${API_SERVER_WORKERS:-1}"
API_SERVER_TIMEOUT="${API_SERVER_TIMEOUT:-1200}"

micromamba run -n server gunicorn main:app \
  -b 0.0.0.0:${API_SERVER_PORT_HTTP} \
  --workers ${API_SERVER_WORKERS} \
  --worker-class uvicorn.workers.UvicornWorker \
  --timeout ${API_SERVER_TIMEOUT}

if [[ "$1" == '-h' ]]; then
  echo "Usage: $0 FASTAPI_HOST FASTAPI_PORT"
  exit 0
fi

if [[ -z "$1" ]]; then
  if [[ -z "${FASTAPI_HOST}" ]]; then
    FASTAPI_HOST="127.0.0.1"
    echo "FASTAPI_HOST set to $FASTAPI_HOST because FASTAPI_HOST and \$1 is undefined"
  else
    FASTAPI_HOST="${FASTAPI_HOST}"
  fi
else
  FASTAPI_HOST=$1
fi

if [[ -z "$2" ]]; then
  if [[ -z "${FASTAPI_PORT}" ]]; then
    FASTAPI_PORT=8080
    echo "FASTAPI_PORT set to $FASTAPI_PORT because FASTAPI_PORT and \$2 is undefined"
  else
    FASTAPI_PORT="${FASTAPI_PORT}"
  fi
else
  FASTAPI_PORT=$2
fi

echo "uvicorn apis.main:app --host $FASTAPI_HOST --port $FASTAPI_PORT"

cd api && uvicorn api.main:app --host $FASTAPI_HOST --port $FASTAPI_PORT


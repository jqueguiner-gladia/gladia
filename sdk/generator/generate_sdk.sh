if [[ "$1" == '-h' ]]; then
  echo "Usage: $0 FASTAPI_HOST FASTAPI_PORT OPENAPI_GENERATOR_VERSION"
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

if [[ -z "$3" ]]; then
  if [[ -z "${OPENAPI_GENERATOR_VERSION}" ]]; then
    OPENAPI_GENERATOR_VERSION="4.0.0"
    echo "OPENAPI_GENERATOR_VERSION set to $OPENAPI_GENERATOR_VERSION because OPENAPI_GENERATOR_VERSION and \$3 is undefined"
  else
    OPENAPI_GENERATOR_VERSION="${OPENAPI_GENERATOR_VERSION}"
  fi
else
  OPENAPI_GENERATOR_VERSION=$3
fi

echo "uvicorn api.main:app --host $FASTAPI_HOST --port $FASTAPI_PORT"

cwd=$PWD
cd ../../api && uvicorn main:app --host $FASTAPI_HOST --port $FASTAPI_PORT &

uvicorn_pid=$!

echo "RUNNING UVICORN WITH PID $uvicorn_pid"

sleep 1

cd $cwd

echo "EXTRACTING ALL CLIENT LANGUAGES"
python get_all_languages.py $OPENAPI_GENERATOR_VERSION

for language in $(cat languages.txt); 
do
  echo "--------------------------"
  echo "BUILDING SDK FOR $language"
  docker run --network=host --rm \
    -v $(pwd)/../clients:/tmp \
    openapitools/openapi-generator-cli:v$OPENAPI_GENERATOR_VERSION generate \
    -i http://$FASTAPI_HOST:$FASTAPI_PORT/openapi.json \
    -o /tmp/$language \
    -D modelDocs=false \
    -D apiDocs=false \
    -D apiTests=false \
    -D modelTests=false \
    -D npmVersion=3.5.2 \
    -D supportsES6=true \
    -g $language \
    --skip-validate-spec

done 

echo "KILLING UVICORN WITH PID $uvicorn_pid"
kill -9 $uvicorn_pid

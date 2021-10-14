if [ -z "$1" ]; then
    env="dev"
else
    env=$1
fi
if [ "$env" = "dev" ] || [ "$env" = "prod" ]; then
    docker build -t unifai/unifai-apis-core -f gpu.Dockerfile.$env .
    docker tag $env unifai/unifai-apis-core
    docker push unifai/unifai-apis-core
fi

docker build -t unifai-apis -f gpu.Dockerfile.dev . && docker run -it -v $PWD:/app -p 8080:80 unifai-apis


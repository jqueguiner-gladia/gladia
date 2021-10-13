docker build -t unifai-apis -f gpu.Dockerfile.prod . && docker run -d -p 8080:80 unifai-apis

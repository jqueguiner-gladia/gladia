docker build -t databuzzword/ai-api-marketplace $1 -f gpu.Dockerfile .
docker tag databuzzword/ai-api-marketplace databuzzword/ai-api-marketplace
docker push databuzzword/ai-api-marketplace

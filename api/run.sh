docker run -it -v $PWD:/app -p 5000:80 -e CUDA_VISIBLE_DEVICE=$1 $2

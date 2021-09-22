docker build -t a -f Dockerfile . && docker run -it -v $PWD:/app -p 5000:80 a

FROM databuzzword/kaggle-gpu

WORKDIR /tmp
RUN wget -O git-lfs.tar.gz https://github.com/git-lfs/git-lfs/releases/download/v2.9.0/git-lfs-linux-amd64-v2.9.0.tar.gz && \
    tar -xvf git-lfs.tar.gz && \
    /bin/bash install.sh

#COPY . /app
COPY requirements.txt /app/

WORKDIR /app

RUN pip3 install -r requirements.txt

RUN pip3 install git+https://github.com/jqueguiner/ai-api-utils.git

ENTRYPOINT [ "uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8081", "--reload" ]

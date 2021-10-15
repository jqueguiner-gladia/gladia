<p align="center">
  <a href="https://unifai.store"><img src="https://i.ibb.co/XLgpJXc/Capture-d-e-cran-2021-10-06-a-09-10-19.png" alt="UnifAI"></a>
</p>
<p align="center">
  <em>UnifAI the fastest way to develop and deploy AI APIs.</em>
  <br/>
  <em>The fastest way to implement and compare AI research models for a given task.</em>
</p>

---

**Documentation**: <a href="https://unifai-1.gitbook.io/unifai/" target="_blank">https://unifai-1.gitbook.io/unifai/</a>

**Documentation Source Code**: <a href="https://github.com/jqueguiner/unifai-docs/" target="_blank">https://github.com/jqueguiner/unifai-docs</a>

**Framework Source Code**: <a href="https://github.com/jqueguiner/unifai-apis-core/" target="_blank">https://github.com/jqueguiner/unifai-apis-core</a>

---

[![CircleCI](https://circleci.com/gh/theunifai/unifai-apis-core/tree/main.svg?style=svg)](https://circleci.com/gh/theunifai/unifai-apis-core/tree/main)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
# Quickstart
## Magic Start
```sh
docker run -d -p 8080:80 unifai/unifai-apis-core:latest
```

## What's behind the Quickstart scene
```sh
git clone https://github.com/jqueguiner/unifai-apis-core.git
cd unifai-apis-core/api

## DEV ENV
#building the development Docker Image
docker build -t unifai-apis -f gpu.Dockerfile.dev .
# OR use our magic wrapper
./build_and_dev.sh

## PROD ENV
#building the production Docker Image
docker build -t unifai-apis -f gpu.Dockerfile.prod .
# OR use our magic wrapper
./build_and_prod.sh
``` 

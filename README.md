<p align="center">
  <a href="https://gladia.io"><img src="https://i.ibb.co/R00fYnT/icon.png" alt="Gladia"></a>
</p>
<p align="center">
  <em>Gladia the fastest way to develop and deploy AI APIs.</em>
  <br/>
  <em>The fastest way to implement and compare AI research models for a given task.</em>
</p>

---
**Open Source Project Website**: <a href="https://www.gladia.io/" target="_blank">https://www.gladia.io/</a>

**Documentation**: <a href="https://docs.gladia.io/" target="_blank">https://docs.gladia.io/</a>

**Documentation Source Code**: <a href="https://github.com/gladiaio/docs/" target="_blank">https://github.com/gladiaio/docs</a>

**Framework Source Code**: <a href="https://github.com/gladiaio/gladia/" target="_blank">https://github.com/gladiaio/gladia</a>

**Discord**: <a href="https://discord.com/invite/S9zFnPNG" target="_blank">https://discord.com/invite/S9zFnPNG</a>

---
<p align="left">
  <a href="https://github.com/gladiaio/gladia/issues" alt="Issues">
    <img src="https://img.shields.io/github/issues/gladiaio/gladia" />
  </a>
  <a href="https://github.com/gladiaio/gladia/pulls" alt="Pull Requests">
    <img src="https://img.shields.io/github/issues-pr/gladiaio/gladia" />
  </a>
  <a href="https://github.com/gladiaio/gladia/network/members" alt="Forks">
    <img src="https://img.shields.io/github/forks/gladiaio/gladia" />
  </a>
  <a href="https://github.com/gladiaio/gladia/stargazers" alt="Stars">
    <img src="https://img.shields.io/github/stars/gladiaio/gladia" />
  </a>
  <a href="https://opensource.org/licenses/MIT" alt="License">
    <img src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  </a>
</p>

<p align="left">
  <a href="https://hub.docker.com/repository/docker/gladiaio/gladia" alt="Dockerhub">
    <img src="https://dockeri.co/image/gladiaio/gladia" />
  </a>
</p>


Follow Gladia Twitter account to get the latest update
<p align="left">
  <a href="https://twitter.com/gladia_io" alt="Twitter">
    <img src="https://img.shields.io/twitter/follow/gladia_io.svg?style=social&label=Follow" />
  </a>
</p>

[Follow Gladia Twitter account to get the latest update](https://twitter.com/gladia_io)

[Join our Discord Community Server](https://discord.gg/2T36ybdDRS)



# Quickstart
## Magic Start
```sh
docker run -d -p 8080:80 gladiaio/gladia:latest
```
Access the service through [http://localhost:8080/docs](http://localhost:8080/docs) or [http://localhost:8080/redoc](http://localhost:8080/redoc) or whatever public/private IP of the server you are running on?

/!\ The First API call of each endpoint might/will be slower as its preforming lazy model caching (after the first call should be ok).

## What's behind the Quickstart scene
```sh
git clone https://github.com/gladiaio/gladia.git
cd gladia/src

#building the Docker Image
docker build -t gladia -f gpu.Dockerfile .

## DEV ENV
docker run -d -p 8080:80 -v $PWD:/app gladiaio/gladia:latest

##PROD ENV
docker run -d -p 8080:80 gladiaio/gladia:latest
```

# CI
CI power by [CDS](https://github.com/ovh/cds)

# Project Management
- [core framework](https://github.com/gladiaio/gladia/projects/1)
- [AI API collection](https://github.com/gladiaio/gladia/projects/2)
- [Issues](https://github.com/gladiaio/gladia/projects/3)
- [Pull Requests](https://github.com/gladiaio/gladia/projects/5)

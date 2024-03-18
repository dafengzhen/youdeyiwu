#!/bin/sh

curl -fsSL -o .env https://raw.githubusercontent.com/dafengzhen/youdeyiwu/main/hosting/.env
curl -fsSL -o docker-compose.yml https://raw.githubusercontent.com/dafengzhen/youdeyiwu/main/hosting/docker-compose.yml
docker compose up -d

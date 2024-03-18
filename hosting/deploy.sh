#!/bin/sh

curl -fsSL https://raw.githubusercontent.com/dafengzhen/youdeyiwu/main/hosting/.env
curl -fsSL https://raw.githubusercontent.com/dafengzhen/youdeyiwu/main/hosting/docker-compose.yml
docker compose up -d

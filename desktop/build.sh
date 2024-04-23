#!/bin/sh

# This is a custom script that builds desktop application files using default settings and Docker.
# If you have custom settings, you may need to update the script accordingly.

echo "Starting to build desktop application files..."

docker build -t youdeyiwu-pc .
docker run --rm -v youdeyiwu-pc-electron:/root/.cache/electron -v youdeyiwu-pc-electron-builder:/root/.cache/electron-builder -v youdeyiwu-pc-out:/youdeyiwu-pc/out youdeyiwu-pc
docker volume inspect --format '{{ .Mountpoint }}' youdeyiwu-pc-out | xargs cd && ll

echo "Build completed."

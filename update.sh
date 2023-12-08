#!/bin/sh

# This is a script that conveniently updates the Docker image for the current project using default settings.
# If you have customized settings, you may need to update the script accordingly.

CONTAINER_ID="$1"

if [ -z "$CONTAINER_ID" ]; then
  echo "Please provide a container ID or 'none' as an argument."
  exit 1
fi

if [ "$CONTAINER_ID" = "none" ]; then
  echo "Skipping container stop and removal."
else
  docker stop "$CONTAINER_ID"
  docker rm "$CONTAINER_ID"
fi

docker build -t youdeyiwu .
docker run --name youdeyiwu --restart=always -d -p 8080:8080 youdeyiwu

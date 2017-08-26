#!/bin/bash -eu

# docker pull alband/crayon
docker run -d -p 8888:8888 -p 8889:8889 --name crayon alband/crayon

echo tensorboard available at http://localhost:8888
echo crayon service available at http://localhost:8889

# build
FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt update && apt install -y libgmp10
ADD root /
ENTRYPOINT ["/bin/bash"]

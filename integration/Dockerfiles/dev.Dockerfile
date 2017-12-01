# build
FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt update && apt install -y libgmp10 wget curl jq nano
ADD root /
ENTRYPOINT ["/bin/bash"]

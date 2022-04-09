FROM rocker/r-ver:4.1.2
RUN apt update && \
    apt install -y --no-install-recommends apt-utils && \
    apt install tzdata -y && \
    echo "Asia/Taipei" > /etc/timezone && \
    ln -fs /usr/share/zoneinfo/Asia/Taipei /etc/localtime && \
    dpkg-reconfigure -f noninteractive tzdata &&\
    apt install build-essential libssl-dev curl libssl-dev libxml2-dev libxml2-dev libfontconfig1-dev libssl-dev libgit2-dev libharfbuzz-dev libfribidi-dev libcurl4-openssl-dev cmake -y
VOLUME /root/.cache/R/AnnotationHub
COPY . /opt/leo
RUN /opt/leo/setup.R
WORKDIR /opt/leo
ENTRYPOINT ["./run.R"]

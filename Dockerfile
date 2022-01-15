ARG UBUNTU_VER=20.04
FROM ubuntu:${UBUNTU_VER}
RUN apt update && \
    apt install -y --no-install-recommends apt-utils && \
    apt install tzdata -y && \
    echo "Asia/Taipei" > /etc/timezone && \
    ln -fs /usr/share/zoneinfo/Asia/Taipei /etc/localtime && \
    dpkg-reconfigure -f noninteractive tzdata &&\
    apt install build-essential libssl-dev curl libssl-dev libxml2-dev libxml2-dev libfontconfig1-dev libssl-dev libgit2-dev -y &&\
    apt install r-base-dev xorg-dev libcurl4-openssl-dev -y && \
    apt install xvfb xauth xfonts-base -y # support png && \
    cd /opt && \
    curl -s https://cran.r-project.org/src/base/R-4/R-4.1.2.tar.gz | tar -zxf - -C /opt && \
    cd R-4.1.2 && \
    ./configure && \
    THREADS=$(lscpu -b -p=Core,Socket | grep -v '^#' | sort -u | wc -l) &&\
    make -j ${THREADS} && make install -j ${THREADS} && cd /opt && \
    rm -rf R-4.1.2 
ENV PKG_CONFIG_PATH=/usr/lib/aarch64-gnu/pkgconfig
RUN R -e 'install.packages(c("shiny", "shinydashboard", "shinythemes", "devtools", "BiocManager", "dplyr", "plyr", "magrittr", "purrr", "httpuv", "rlang", "tibble","car", "ufs", "FSA", "corrplot", "emmeans",  "readxl", "rstatix", "parallel", "rentrez"), repos = "https://cloud.r-project.org", dependencies = TRUE)'
RUN R -e 'BiocManager::install(c( "AnnotationHub", "BiocGenerics", "DOSE","clusterProfiler", "fgsea"), site_repository="https://bioconductor.org/packages/3.14/bioc", ask=FALSE, force=TRUE)' &&\
    echo "Finished"
COPY . /opt/leo
WORKDIR /opt/leo
ENTRYPOINT ["Rscript run.R"]

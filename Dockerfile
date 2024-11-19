FROM mambaorg/micromamba:2.0.3
COPY --chown=$MAMBA_USER:$MAMBA_USER conda.lock.yml /tmp/env.yaml
RUN micromamba install -y -n base -f /tmp/env.yaml && \
    micromamba clean --all --yes
COPY . /opt/leo
VOLUME /home/$MAMBA_USER/.cache/R/AnnotationHub
ENTRYPOINT ["/usr/local/bin/_entrypoint.sh", "/opt/leo/run.R"]
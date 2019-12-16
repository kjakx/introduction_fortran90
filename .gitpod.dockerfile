FROM gitpod/workspace-full:latest

USER root
# Install custom tools, runtime, etc.
RUN     export DEBIAN_FRONTEND=noninteractive \
        apt-get -E update && apt-get -E install -y \
        gfortran gnuplot\
    && apt-get clean && rm -rf /var/cache/apt/* && rm -rf /var/lib/apt/lists/* && rm -rf /tmp/*

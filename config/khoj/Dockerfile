# syntax=docker/dockerfile:1
FROM ubuntu:jammy
LABEL homepage="https://khoj.dev"
LABEL repository="https://github.com/khoj-ai/khoj"
LABEL org.opencontainers.image.source="https://github.com/khoj-ai/khoj"

# Install cuda
ENV NVARCH=x86_64
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    gnupg2 curl ca-certificates && \
    curl -fsSLO https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2204/${NVARCH}/cuda-keyring_1.1-1_all.deb && \
    dpkg -i cuda-keyring_1.1-1_all.deb && \
    apt-get purge --autoremove -y curl \
    && rm -rf /var/lib/apt/lists/*

# For libraries in the cuda-compat-* package: https://docs.nvidia.com/cuda/eula/index.html#attachment-a

RUN apt-get update && apt-get install -y --no-install-recommends \
    cuda-12-1 cuda-toolkit-12-1 \
    && rm -rf /var/lib/apt/lists/*

# Required for nvidia-docker v1
RUN echo "/usr/local/nvidia/lib" >> /etc/ld.so.conf.d/nvidia.conf \
    && echo "/usr/local/nvidia/lib64" >> /etc/ld.so.conf.d/nvidia.conf

ENV PATH=/usr/local/cuda/bin:${PATH}
ENV LD_LIBRARY_PATH=/usr/local/cuda/lib:/usr/local/cuda/lib64

# Install System Dependencies
RUN apt update -y && apt -y install python3-pip swig curl git

# Install Node.js and Yarn
RUN curl -sL https://deb.nodesource.com/setup_20.x | bash -
RUN apt -y install nodejs
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt update && apt -y install yarn

# Install RapidOCR dependencies
RUN apt -y install libgl1 libgl1-mesa-glx libglib2.0-0

# Install Application
RUN git clone https://github.com/khoj-ai/khoj /app
WORKDIR /app
RUN CMAKE_ARGS="-DGGML_CUDA=on" pip install --no-cache-dir .

# Set the PYTHONPATH environment variable in order for it to find the Django app.
ENV PYTHONPATH=/app/src:$PYTHONPATH

# Go to the directory src/interface/web and export the built Next.js assets
WORKDIR /app/src/interface/web
RUN bash -c "yarn cache clean && yarn install --verbose && yarn ciexport"
WORKDIR /app

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}
ENTRYPOINT ["python3", "src/khoj/main.py"]


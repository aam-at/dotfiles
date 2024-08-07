# syntax=docker/dockerfile:1
FROM nvidia/cuda:12.1.1-cudnn8-devel-ubuntu22.04
LABEL homepage="https://khoj.dev"
LABEL repository="https://github.com/khoj-ai/khoj"
LABEL org.opencontainers.image.source="https://github.com/khoj-ai/khoj"

# Environment variables
ENV REPO_URL="https://github.com/khoj-ai/khoj"
ENV APP_DIR="/app"
ENV PYTHONPATH="${APP_DIR}/src:${PYTHONPATH}"

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
RUN git clone ${REPO_URL} ${APP_DIR}
WORKDIR /app
ARG VERSION=0.0.0
RUN sed -i "s/dynamic = \\[\"version\"\\]/version = \"$VERSION\"/" pyproject.toml && \
    CMAKE_ARGS="-DGGML_CUDA=on" pip install --no-cache-dir .

# Go to the directory src/interface/web and export the built Next.js assets
WORKDIR /app/src/interface/web
RUN bash -c "yarn cache clean && yarn install --verbose && yarn ciexport"
WORKDIR /app

# Run the Application
# There are more arguments required for the application to run,
# but these should be passed in through the docker-compose.yml file.
ARG PORT
EXPOSE ${PORT}

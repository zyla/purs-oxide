FROM node:16.14.0-bullseye-slim

RUN apt-get update \
  && apt-get -y install neovim sudo less wget git locales postgresql-client tig \
  && rm -rf /var/cache/apt/*

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && locale-gen \
 && echo "node ALL=NOPASSWD: ALL" > /etc/sudoers.d/60-user

ENV LC_ALL en_US.UTF-8
USER node

RUN echo 'set -o vi' >> ~/.bashrc
ENV PATH ./node_modules/.bin:${PATH}

RUN sudo wget -O /usr/local/bin/dumb-init https://github.com/Yelp/dumb-init/releases/download/v1.2.4/dumb-init_1.2.4_x86_64 \
 && sudo chmod +x /usr/local/bin/dumb-init \
 && mkdir -p ~/.config \
 && ln -sf ~/.persistent/config/nvim ~/.config/nvim


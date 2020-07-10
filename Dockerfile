# syntax = docker/dockerfile:experimental

# https://docs.docker.com/engine/reference/builder/#from
#   "The FROM instruction initializes a new build stage and sets the
#    Base Image for subsequent instructions."
FROM erlang:21.3.8.7-alpine as service-builder
# https://docs.docker.com/engine/reference/builder/#label
#   "The LABEL instruction adds metadata to an image."
LABEL stage=builder

# the following argument will allow to delete the stages that
# become too old (like in jenkins)
ARG BUILD_ID
LABEL build=$BUILD_ID

# located in the image
WORKDIR /apps/service
ENV REBAR_BASE_DIR /apps/service/_build

# Install git for fetching non-hex depenencies.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    mkdir ~/.ssh && \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk update && \
    apk add --update openssh-client git bash

# this local ssh config file contains the all the necessary
# ssh configuration of host/keys
# configuration that allows rebar3 to fetch private deps
# eg.
#   Host external-repo external-repo.com
#     HostName external-repo.com
#     IdentityFile ~/.ssh/external-repo-pem.key
#
#   StrictHostKeyChecking no
#
# also notice that you will need to link/copy the .pem of your external
# dependency repo to the root of this project so the --mount directive below
# can find it
COPY docker/ssh-config /root/.ssh/config

# create a new deps compiling layer for later re-use
FROM service-builder as service-deps-compiler
LABEL stage=deps-compiler

# build and cache dependencies as their own layer
# every time you change something in the rebar.config or rebar.lock
# this layer will get rebuilt
COPY rebar.config rebar.lock .
RUN --mount=id=hex-cache,type=cache,sharing=shared,target=/root/.cache/rebar3 \
    # the following line is how you transport secrets into the container, one
    # such example for a secret is the .pem key that allows fetch of private deps
    # from a repo as in the example above
    --mount=type=secret,id=stash-miniclip-com-pem.key,target=/root/.ssh/stash-miniclip-com-pem.key \
    --mount=type=ssh \
    rebar3 as docker compile

# create a new app layer
FROM service-deps-compiler as service-app-compiler
LABEL stage=app-compiler

# now that dependencies are built, build the application in it's own layer,
# we copy everything that's not docker ignored into the work dir, this
# means the actual application and not the dependencies (look up .dockerignore)
COPY . .

RUN --mount=id=hex-cache,type=cache,sharing=shared,target=/root/.cache/rebar3 \
    # same comment as the one above
    --mount=type=secret,id=stash-miniclip-com-pem.key,target=/root/.ssh/stash-miniclip-com-pem.key \
    --mount=type=ssh \
    rebar3 as docker release

# create a new release layer
FROM service-app-compiler as service-releaser
LABEL stage=releaser

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

# install tar for unpacking the target system
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    apk add --update tar

# generate the release tarball and unpack it to the target dir
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as docker tar && \
    tar -zxvf $REBAR_BASE_DIR/docker/rel/*/*.tar.gz -C /opt/rel

# this is the final runner layer, notice how it diverges from the original erlang
# alpine layer, this means this layer won't have any of the other stuff that was
# generated previously (deps, build, etc)
FROM erlang:21.3.8.7-alpine as service-runner

# final location of the service
WORKDIR /srv/service

ENV COOKIE=service.cookie \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp

# openssl needed by the crypto app
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update bash openssl ncurses openssh openssh-keygen

# copy the untarred release from the release layer here
COPY --from=service-releaser /opt/rel .

# at last, spin up the node
ENTRYPOINT ["/srv/service/bin/simple_web_server"]
CMD ["foreground"]


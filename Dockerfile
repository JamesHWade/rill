ARG OAUTH2_PROXY_IMAGE=quay.io/oauth2-proxy/oauth2-proxy:v7.15.4-alpine@sha256:77a608f6da587bda7fe1371c503dc89b39d0fb9d78dc3d47c1ef253abfc9d2bd
FROM ${OAUTH2_PROXY_IMAGE} AS oauth2-proxy

FROM rocker/r-ver:4.6.1@sha256:a5df0ae591422cc1733e97da03a9f4eff4cf172e40895225959aa93bb7ff7517

ARG RILL_SOURCE_REVISION=unknown

LABEL org.opencontainers.image.source="https://github.com/JamesHWade/rill" \
      org.opencontainers.image.revision="${RILL_SOURCE_REVISION}"

ENV DEBIAN_FRONTEND=noninteractive \
    R_ENVIRON_USER=/dev/null \
    R_PROFILE_USER=/dev/null \
    PKG_SYSREQS=true

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
      ca-certificates \
      curl \
      g++ \
      git \
      libcurl4-openssl-dev \
      libpq-dev \
      libssl-dev \
      libxml2-dev \
      make \
      pkg-config \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R --quiet -e \
    'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'

WORKDIR /opt/rill/src

COPY DESCRIPTION NAMESPACE ./

RUN R --quiet -e 'pak::local_install_deps("/opt/rill/src", dependencies = c("Depends", "Imports", "LinkingTo"), ask = FALSE)'

COPY . ./

RUN R --quiet -e 'pak::local_install("/opt/rill/src", dependencies = FALSE, upgrade = FALSE, ask = FALSE)' \
    && rm -rf \
      /opt/rill/src \
      /root/.cache/R \
      /root/.cargo \
      /root/.rustup \
      /tmp/* \
      /usr/local/lib/R/site-library/pak

COPY docker /opt/rill/docker

COPY --from=oauth2-proxy /bin/oauth2-proxy /usr/local/bin/oauth2-proxy

RUN groupadd --gid 10001 rill \
    && useradd \
      --uid 10001 \
      --gid 10001 \
      --create-home \
      --shell /usr/sbin/nologin \
      rill

ENV HOME=/home/rill \
    RILL_SHINY_PORT=3838

USER 10001:10001
WORKDIR /home/rill

EXPOSE 10000

HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD ["/opt/rill/docker/healthcheck.sh"]

ENTRYPOINT ["/opt/rill/docker/entrypoint.sh"]
CMD ["web"]

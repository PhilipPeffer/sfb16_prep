FROM rocker/rstudio:4.6.0

ENV DISABLE_AUTH=true

# Install system dependencies for R packages and Quarto
RUN apt-get update && apt-get install -y \
    curl \
    wget \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    libsodium-dev \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Quarto
RUN QUARTO_VERSION=1.6.42 && \
    wget -q "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb" && \
    dpkg -i "quarto-${QUARTO_VERSION}-linux-amd64.deb" && \
    rm "quarto-${QUARTO_VERSION}-linux-amd64.deb"

# Pre-install renv and languageserver into the system library
RUN R -e "install.packages(c('renv', 'languageserver'), repos = 'https://cloud.r-project.org')"

# Set default working directory in RStudio
RUN echo 'session-default-working-dir=/workspaces/sfb16_prep' >> /etc/rstudio/rsession.conf

WORKDIR /workspaces/sfb16_prep

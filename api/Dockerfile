FROM trestletech/plumber
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get install libmariadbclient-dev -y
RUN apt-get update -y
RUN apt-get install -y mysql-client
RUN apt-get install -y r-cran-rcppeigen
RUN \
  apt-get update && apt-get install -y apt-transport-https && \
  apt-get install -y python python-dev python-pip python-virtualenv libssl-dev libjpeg-dev libmagick++-dev && \
  rm -rf /var/lib/apt/lists/*
RUN R -e 'install.packages("RMySQL",dependencies=T)'
RUN R -e 'install.packages("ranger",dependencies=T)'
RUN R -e 'install.packages("keras",dependencies=T, type = "source")'
RUN R -e 'library(keras);install_keras()'
RUN mkdir -p /app/
RUN mkdir -p /models/
WORKDIR /app/
COPY api.R /app/
CMD ["/app/api.R"]

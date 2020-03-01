FROM maurony/mro-ts-forecasting:latest

ENV CURL_CA_BUNDLE /opt/microsoft/ropen/3.5.1/lib64/R/lib/microsoft-r-cacert.pem

# use littler in the future to install packages

# azureparallel libraries - find a solution for the curl path
RUN Rscript -e "setwd('/opt/microsoft/ropen/3.5.1/lib64/R/lib/'); library(devtools); options(unzip = 'internal'); devtools::install_github(repo = 'yvesmauron/forunco', ref = 'master')" 

#RUN install2.r --error \
#    --deps TRUE \
#    RevoScaleR \

LABEL org.label-schema.license="Founco" \
    	org.label-schema.vendor="Forecasting library of Founco, Dockerfile provided by Yves Mauron" \
	org.label-schema.name="Forunco" \
	org.label-schema.description="Forunco." \ 
	org.label-schema.vcs-url=$VCS_URL \
	org.label-schema.vcs-ref=$VCS_REF \
	org.label-schema.build-date=$BUILD_DATE \
	org.label-schema.schema-version="rc1" \
	maintainer="Yves Mauron <ypmauron@gmail.com>"

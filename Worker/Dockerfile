FROM rocker/tidyverse:latest

RUN apt-get update && apt-get install -y \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
libudunits2-dev \
libprotobuf-dev \
protobuf-compiler \ 
libproj-dev \
libgdal-dev \
libmagick++-dev \ 
    && rm -rf /var/lib/apt/lists/*
    
RUN mkdir -p /home/cm
RUN cd /home/cm


RUN install2.r --error RSocrata tidyverse tidygeocoder mapboxapi lubridate httr jsonlite aws.s3 


RUN ls -al '/home/cm'

ADD Worker.R /home/cm/

EXPOSE 2000



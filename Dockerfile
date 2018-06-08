FROM openanalytics/r-base

MAINTAINER Carsten Behring "carsten.behring@efsa.europa.eu"

RUN apt-get update && apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev openjdk-8-jdk libgsl-dev libpoppler-cpp-dev
RUN R CMD javareconf
RUN install2.r -e rJava shiny caretEnsemble recipes  plyr xlsx tm DT ranger SnowballC wordcloud e1071 kernlab caret topicmodels pROC DMwR randomForest hmeasure pdftools gbm ROSE devtools

RUN Rscript -e 'devtools::install_github(c("zachmayer/caretEnsemble", "ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"))'

RUN mkdir /root/AbstractScreening
COPY AbstractScreening_0.1.0.tar.gz /root/
RUN R CMD INSTALL /root/AbstractScreening_0.1.0.tar.gz

EXPOSE 3838

CMD ["R", "-e", "AbstractScreening::runShiny(port=3838,host='0.0.0.0')"]





#### Define global options
options(shiny.error=traceback, stringsAsFactors=FALSE)

#### Load dependencies
library(shiny)
library(shinyBS)
library(plyr)
library(dplyr)
library(RColorBrewer)

#### Define global parameters
colpals=c("Dark2")
cols=unlist(Map(brewer.pal, brewer.pal.info[match(colpals, rownames(brewer.pal.info)),1], colpals))

#### Define functions and classes
source("classes.R")
source("functions.R")


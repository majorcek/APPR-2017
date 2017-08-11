library(knitr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(shiny)
library(RCurl)
library(XML)
#library(rJava)
#library(xlsx)
library(rmarkdown)
library(rgdal)
library(ggvis)
library(RColorBrewer)
library(sp)
library(rgeos)
 

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")

library(readxl)

read.xlsx <- function(path, sheetIndex = NULL, header = TRUE, encoding = NULL) {
  return(read_xlsx(path, sheet = sheetIndex, col_names = header))
}


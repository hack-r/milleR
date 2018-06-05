#' Some common libs
#'
#' @param code boolean, defaults to FALSE. T = print package list. 
#' @return None
#' @export
#' 

dsLibs <- function(code=F){
  # This is NOT meant to be any sort of comprehensive library list
  
  # These are just some of the most common libs I use 
  
  if(!require("pacman")){
    install.packages("pacman")
    require(pacman)
  }
  
  pkgs <- c("arules",
            "caret", 
            "data.table", 
            "dplyr", 
            "e1071", 
            "forecast",
            "ggplot2",
            "h2o", 
            "h2oEnsemble",
            "imputeTS",
            "lubridate", 
            "Matrix",
            "Metrics",
            "ModelMetrics",
            "odbc", 
            "recipes",
            "RODBC", 
            "RRF", 
            "speedglm", 
            "sqldf", 
            "tidyjson",
            "tidyverse", 
            "tidytext", 
            "tm",
            "xgboost", 
            "xts")
  
  eval(parse(text=paste0("pacman::p_load(", pkgs,")")))
  
  if(code ==T){
      cat(pkgs)
  }
}

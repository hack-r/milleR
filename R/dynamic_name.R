#' Auto generate code to write out files with the date
#'
#' @param x character, scalar. A custom file name or Folder/in/Balitomore/name 
#' @return None
#' @export
#' 

dynamic_name <- function(x){
  # This just gives you the code to copy and paste
  
  tmp         <- gsub("modeling_",x,
        'todays_date <- Sys.Date();
train <- paste0("modeling_",
                                     lubridate::month(todays_date, label=T,abbr=T),
       "_",
       lubridate::day(todays_date),
       "_",
       lubridate::year(todays_date),
       ".RDS")
       ')
  cat(tmp)
}

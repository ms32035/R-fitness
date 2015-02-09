#' @export
import_steps <- function(filename) {
  
  json <- fromJSON(filename)
  
  vals <- unlist(json$point$value)
  time_df <- lapply(lapply(lapply(json$point[,c(1,2)],substr,1,10),as.numeric),as.POSIXct,origin = "1970-01-01")
  names(time_df) <- c('startTime','endTime')
  
  return (data.frame(start_time = time_df[1], end_time = time_df[2],steps = vals))
}

#' @export
import_cadence <- function(filename) {
  
  json <- fromJSON(filename)
  
  vals <- unlist(json$point$value)
  time_df <- lapply(lapply(lapply(json$point[,c(1,2)],substr,1,10),as.numeric),as.POSIXct,origin = "1970-01-01")
  names(time_df) <- c('startTime','endTime')
  
  return (data.frame(start_time = time_df[1], end_time = time_df[2],cadence = vals))
}
#' @export
import_distance <- function(filename) {
  
  json <- fromJSON(filename)
  
  vals <- unlist(json$point$value)
  time_df <- lapply(lapply(lapply(json$point[,c(1,2)],substr,1,10),as.numeric),as.POSIXct,origin = "1970-01-01")
  names(time_df) <- c('startTime','endTime')
  
  return (data.frame( time_df[1],  time_df[2],val = vals))
}
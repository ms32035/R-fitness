#' @export
row_dependencies <- function (distance, location, activity, steps, candence) { 
  
  print(paste('Distance:',nrow(distance)))
  print(paste('Location:',nrow(location)))
  print(paste('Activity:',nrow(activity)))
  print(paste('Steps:',nrow(steps)))
  print(paste('Cadence:',nrow(cadence)))
  print(paste('Distance + Location (startTime + endTime):', nrow(merge(distance,location, by=c("startTime","endTime")))))
  print(paste('Distance + Location (startTime):', nrow(merge(distance,location, by="startTime"))))
  print(paste('Distance + Location (endTime):', nrow(merge(distance,location, by="endTime"))))
  print(paste('Distance + Activity (startTime):', nrow(merge(distance,activity, by="startTime"))))
  print(paste('Distance + Activity (endTime):', nrow(merge(distance,activity, by="endTime"))))
  print(paste('Location + Activity (startTime):', nrow(merge(location,activity, by="startTime"))))
  print(paste('Location + Activity (endTime):', nrow(merge(location,activity, by="endTime"))))
  print(paste('Distance + Steps (startTime):', nrow(merge(distance,steps, by="startTime"))))
  print(paste('Location + Steps (startTime):', nrow(merge(location,steps, by="startTime"))))
  print(paste('Activity + Steps (startTime):', nrow(merge(activity,steps, by="startTime"))))
  print(paste('Activity + Steps (endTime):', nrow(merge(activity,steps, by="endTime"))))
  print(paste('Steps + Cadence (startTime + endTime):', nrow(merge(steps,cadence, by=c("startTime","endTime")))))
  print(paste('Steps + Cadence (startTime):', nrow(merge(steps,cadence, by="startTime"))))
  print(paste('Steps + Cadence (endTime):', nrow(merge(steps,cadence, by="endTime"))))
}

#' @export
create_bins <- function(sourceData,startTimeColumn,endTimeColumn,valueColumn,nbins=24) {
  
  binValues <- matrix(0,nrow = nbins, ncol = 7)
  binSize <- 60*24 / nbins
  
  for(i in 1:nrow(sourceData)){    
        
    startTime <- sourceData[i,startTimeColumn]
    startDay <- as.numeric(strftime(startTime,'%u'))
    startBinDay <- (as.numeric(difftime(startTime,trunc(startTime,units = "day"),units = "mins")) %/% binSize) + 1
    startBin <- (startDay - 1) * nbins + startBinDay
    
    endTime <- sourceData[i,endTimeColumn]
    endDay <- as.numeric(strftime(endTime,'%u'))
    endBinDay <- (as.numeric(difftime(endTime,trunc(endTime,units = "day"),units = "mins")) %/% binSize) + 1
    endBin <- (endDay - 1) * nbins + endBinDay
    
    binNum <- endBin - startBin +1
    
    for (j in startBin:endBin)
      binValues[j] <- binValues[j] + sourceData[i,valueColumn] / binNum
    
  }
  
  return (binValues)
  
}

#' @export
opt_cluster <- function(x, k_max=10,k_chosen=10, ...) {
  
  if (!require(cluster)) stop("cluster PACKAGE MISSING")
  asw <- numeric(k_max)
  
  for (k in 2:k_max) {  
    asw[k] <- clara(x, k, ...)$silinfo$avg.width
  }
  
  k.best <- which.max(asw)
  
  print(paste("OPTIMAL-K", k.best, sep=": "))
    
  plot(1:k_max, asw, type="s", main="Clustering Optimization using K-Mediods",
       xlab="K (number of clusters)", ylab = "mean silhouette width")
  axis(1, k.best, paste("best",k.best,sep="\n"), col="red",col.axis="red")
    
  return(clara(x, k_chosen, ...))

}
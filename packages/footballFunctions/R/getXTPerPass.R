getXTPerPass <- function(OneGame){
  grid <- read.csv("https://raw.githubusercontent.com/RobinKoetsier/temp/master/xT.csv",header=FALSE, sep=";")
  data <- OneGame

  colnames(grid)<-round(seq(0,93.75,(100/16)),4)
  rownames(grid)<-round(seq(0,(100-(100/12)),(100/12)),4)
  
  #rownames(grid)<-round(seq(from=(100),to=(100/12),by=-(100/12)),4)
  grid[ order(row.names(grid)), ]
  
  data$xTStart <- sapply(seq(nrow(data)), function(i) {
    grid[rev(which(as.numeric(rownames(grid)) <= data$y[i]))[1],
         rev(which(as.numeric(colnames(grid)) <= data$x[i]))[1]]
  })
  
  data$xTEnd <- sapply(seq(nrow(data)), function(i) {
    grid[rev(which(as.numeric(rownames(grid)) <= data$endY[i]))[1],
         rev(which(as.numeric(colnames(grid)) <= data$endX[i]))[1]]
  })
  
  data$net <- as.numeric(unlist(data$xTEnd)) - as.numeric(unlist(data$xTStart))
  
  return(data)
}

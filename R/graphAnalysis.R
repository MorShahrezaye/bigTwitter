#' Converting a list of users containing their friends or foloowrs to an igraph network
#'
#' This function takes multiple lists as input. Each element of the list is a user
#' and each element contains the list of friend or follower ids.
#'
#' @param directed to create a directed network or indirected
#' @return An igraph network
#' @export
userEdgeList2graph <- function(..., directed){
  require(igraph)
  require(data.table)
  networkData <- c(...)
  networkData <- networkData[unlist(lapply(networkData, function(x) length(x)))!=0]

  df <- lapply(names(networkData), function(x) data.table(from = x, to = networkData[[x]]))
  df <- rbindlist(df)
  df2 <- df[!duplicated(df),]
  message(nrow(df)-nrow(df2), " duplicated edges removed")
  g <- graph_from_data_frame(df, directed = directed)
  return(g)
}

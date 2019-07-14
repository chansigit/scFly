#' FindNNIndex: Find the nearest neighbour of query in the reference in hamming space
#'
#' @param query.vector a vector to search
#' @param reference.matrix rows of vectors to query in
#'
#' @return the row index of the search result in the reference matrix
#' @export
#'
#' @examples
FindNNIndex<- function(query.vector, reference.matrix){
  library("e1071")

  hamming.distance.vector <- (apply( reference.matrix,1,hamming.distance,y =query.vector) )

  which.min(hamming.distance.vector)
}



#' SearchCell: Given cell's short encoding and long encoding, search its most similar cell
#'
#' @param query.shortEncoding a short binary vector for multi-probe searching
#' @param query.longEncoding  a long binary vector for searching
#' @param referenceObject a group of cells for reference
#'
#' @return the row name of the obtained cell
#' @export
#'
#' @examples
SearchCell <- function(query.shortEncoding, query.longEncoding, referenceObject){
  key    <- Hashtable.Key(query.shortEncoding)
  rowidx <- referenceObject$hashtable[[key]]
  if (is.null(rowidx)){
    return (NULL)
  }

  reference.longEncodingMatrix <- referenceObject$encode.long[rowidx,]
  foundIdx <- FindNNIndex(query.vector=query.longEncoding, reference.longEncodingMatrix)

  return (rownames(reference.longEncodingMatrix)[foundIdx])
}

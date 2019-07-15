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
  result<-which.min(hamming.distance.vector)
  return(result)
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

  reference.longEncodingMatrix <- referenceObject$encode.long[rowidx,,drop=F]

  foundIdx <- FindNNIndex(query.vector=query.longEncoding,
                          reference.matrix=reference.longEncodingMatrix)
  # needs to be rewrite
  #  foundRow <- reference.longEncodingMatrix[foundIdx,]
  annotated<-(rownames(reference.longEncodingMatrix))[foundIdx]
  return(annotated)
}

#' query.one.cell: a wrapper for query operations
#'
#' @param idx row index of cell in the query matrix
#'
#' @return
#' @export
#'
#' @examples
query.one.cell <- function(idx){
  index <- SearchCell(query.shortEncoding=query$encode.short[idx,],
                      query.longEncoding =query$encode.long[idx,],
                      referenceObject    =reference)
  return(index)
}

#' MappingCells: apply query operations on the query matrix
#'
#' @param query query matrix
#' @param reference reference matrix
#' @param reference.label labels of the reference
#' @param cores number of cores used
#'
#' @return a vector of transfered labels after mapping
#' @export
#'
#' @examples
MappingCells <- function(query, reference, reference.label, cores){
  query.one.cell <- function(idx,reference){
    label <- SearchCell(query.shortEncoding=query$encode.short[idx,],
                        query.longEncoding =query$encode.long[idx,],
                        referenceObject    =reference)
    return(label)
  }
  if (cores>1){
    library(parallel)
    cl = makeCluster(cores)
    # load the packages into all slave processes
    clusterEvalQ(cl=cl, library("scFly"))
    # make variables visible in the work-horse function
    clusterExport(cl=cl, c("query","reference"))
    rowid<-1:nrow(query$encode.short)
    index <- parSapply(cl, rowid, query.one.cell, reference=reference, simplify=TRUE)
    stopCluster(cl)
  }else{
    rowid<-1:nrow(query$encode.short)
    index <- sapply(rowid,query.one.cell,reference=reference, simplify=TRUE)
  }
  return(index)
  # need rewriting
  #return(reference.label[as.numeric(unlist(index))])
}

#' FindCandidateIndex: Find the candidates based on short encodings
#'
#' @param query.vector a binary short-coded vector to search
#' @param reference.matrix a matrix of binary short-coded vector to search against
#' @param minCandidate minimum number of elements in the candidate list
#' @param radius hamming distance threshold radius
#'
#' @return the row index of the search result in the reference matrix
#' @export
#'
#' @examples
FindCandidateIndex<- function(query.vector, reference.matrix, minCandidate, radius = 1){
  library("e1071")
  dist       <- apply( reference.matrix,1,hamming.distance,y =query.vector)
  candidates <- c()
  repeat{
    candidates <- which(dist<=radius)
    if( length(candidates)>minCandidate ){
      break
    }

    if (radius==0){
      radius<-radius+1
    }else{
      radius <- radius*2
    }
  }
  return(candidates)
}

#' FindNearestNeighbour: Given cell's short encoding and long encoding, search its most similar cell
#'
#' @param query.shortEncoding a short binary vector for multi-probe searching
#' @param query.longEncoding a long binary vector for searching
#' @param referenceObject a group of cells for reference
#' @param radius hamming distance threshold radius
#' @param topN number of nearest neighbours returned
#' @param return.simple whether the function returns row indices only
#'
#' @return the row name of the obtained item
#' @export
#'
#' @examples
FindNearestNeighbour <- function(query.shortEncoding,
                                 query.longEncoding,
                                 referenceObject,
                                 radius=2,
                                 topN  =5,
                                 return.simple=FALSE){

  # Keep similar vectors based on their short encodings
  candidateIndex <- FindCandidateIndex(query.vector     = query.shortEncoding,
                                       reference.matrix = referenceObject$encode.short,
                                       minCandidate=topN, radius=radius) #minCandidate is important, or NA occurring in topCandidate.index would cause error in row selections
  # Obatain a candidate vector subset to perform long-encoding query
  reference.longEncodingMatrix <- referenceObject$encode.long[candidateIndex,,drop=F]

  # Calculate hamming distances between the query and the candidates with long encondings
  dist <- apply(reference.longEncodingMatrix, 1, hamming.distance, y=query.longEncoding)

  # Get most similar items from the subset
  ## use sort with index.return=TRUE to return the sorted value with the index in a list
  lst <- sort(dist, index.return=TRUE, decreasing=FALSE)
  topCandidate.index    <- lst$ix[1:topN] # note that these indices are only meaningful in the candidate subset
  topCandidate.dist     <- lst$x [1:topN]
  #print(topCandidate.index)
  #print(topCandidate.dist)
  topCandidate.rowid <- as.character(rownames(reference.longEncodingMatrix[topCandidate.index,,drop=F])) # would cause error if topCandidate.index have NA values

  if (return.simple){
    return(topCandidate.rowid)
  }else{
    output<-data.frame("rowid"=topCandidate.rowid, "dist"=topCandidate.dist, stringsAsFactors = FALSE)
    return(output)
  }
}



#' FindAllNearestNeighbour: apply 'FindNearestNeighbour' to every rows of query
#'
#' @param query an element-by-feature query matrix
#' @param reference an element-by-feature reference matrix
#' @param radius radius for short-encoding threshold
#' @param topN number of nearest neighbours to return
#' @param ncores number of cores used in parallelization (currently not supported :(, sorry )
#'
#' @return Searching results, a dataframe including the row indices of found elements and distances
#' @export
#'
#' @examples
FindAllNearestNeighbour <- function(query, reference,
                                    radius=2, topN  =5, ncores=1){
  library("rlist")
  one.query <- function(idx, reference){
    FindNearestNeighbour(query.shortEncoding=query[["encode.short"]][idx,],
                         query.longEncoding =query[["encode.long"]] [idx,],
                         referenceObject=reference,
                         radius=radius, topN=topN, return.simple=FALSE) -> result

    query.out<-cbind(data.frame(t(result$rowid),stringsAsFactors = F),
                     data.frame(t(result$dist))  )
    colnames(query.out)<-c(paste("nn_rowid",1:nrow(result),sep=""),
                           paste("nn_dist", 1:nrow(result),sep="")  )
    query.out["query"]<- idx

    query.out
  }

  rowid<-1:nrow(query$encode.short)
  if (ncores<=1){
    lst  <-lapply(rowid,one.query,reference=reference)
    return(list.rbind(lst))
  }else{# we don't support parallelization currently :(
    lst  <-lapply(rowid,one.query,reference=reference)
    return(list.rbind(lst))
  }


}

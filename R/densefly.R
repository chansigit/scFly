#' Convert an input vector into activations
#'
#' @param data an input matrix, each row is a data entry
#' @param embedding.size length of embedded vector, i.e. number of columns after embedding
#' @param sampling.rate the fraction of input vector used to calculate one activation
#'
#' @return a matrix with the same number of rows as the input, each row is an embedded vector
#' @export
#'
#' @examples
GetActivations <- function(data, embedding.size, sampling.rate){
  # Generate a random projection matrix, nRow=rr, nCol=cc
  rr            <- ncol(data)      # dimensionality of data
  cc            <- embedding.size  # dimensionality of embedded data
  rand.proj.mat <- matrix(rbinom(rr*cc,1,sampling.rate),rr,cc) # binary random projection matrix

  # Calculate activations
  activations   <- data %*% rand.proj.mat # N-by-d * d-by-mk = N-by-mk
  rownames(activations)<-rownames(data)
  return(activations)
}





#' GetLongHash
#'
#' @param activations an activation matrix, each row represents a data entry's activations
#'
#' @return a binary matrix, each row is an embeded data entry
#' @export
#'
#' @examples
GetLongHash <- function(activations){
  data.embedded <- (activations>=0)
  rownames(data.embedded) <- rownames(activations)
  return(data.embedded+0)
}





#' GetShortHash
#'
#' @param activations an activation matrix, each row represents a data entry's activations
#' @param nProj the number of random projections
#'
#' @return a binary matrix, each row is an embeded data entry
#' @export
#'
#' @examples
GetShortHash<- function(activations, nProj){
  # Calculate group sum of activations
  cell.id<- rownames(activations)

  data.shortly.embedded<- NULL
  for (i in 1:nrow(activations)){
    activation          <- activations[i, ]
    activation.aggr     <- tapply( activation, (seq_along(activation)-1) %/% nProj, sum)
    data.shortly.embedded <- rbind(data.shortly.embedded, (activation.aggr>=0))
  }
  rownames(data.shortly.embedded) <- cell.id
  return(data.shortly.embedded+0)
}





#' BuildEmbeddedObject
#'
#' @param data an input matrix, each row is a data entry
#' @param hash.length the length of an input vector after one random projection, the embedding size will be hash.length*nProj
#' @param nProj the number of random projections
#' @param sampling.rate the fraction of input vector used to calculate one activation
#' @param do.center set this to TRUE if you hope to zero-centralize the data
#'
#' @return a list of embedding results `@long` stores the long hash, `@short` stores the short hash, `@activations` stores the activations
#' @export
#'
#' @examples
BuildEmbeddedObject <- function(data, hash.length, nProj, sampling.rate, do.center=FALSE){
  if (do.center){
    data <- data-rowMeans(data)
  }

  embedding.size<- hash.length* nProj
  activations   <- GetActivations(data, embedding.size, sampling.rate= sampling.rate)
  longHashed    <- GetLongHash(activations)
  shortHashed   <- GetShortHash(activations, nProj)

  # Return a list of embeddings
  require("tictoc")
  print(paste(nrow(data), "data items are processed"))
  return (list("long"=longHashed,
               "short"=shortHashed,
               "activations"=activations))
}


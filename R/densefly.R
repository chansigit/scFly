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

  start_time <- Sys.time()
    embedding.size <- hash.length* nProj
    activations    <- GetActivations(data, embedding.size, sampling.rate= sampling.rate)
    encode.long    <- GetLongHash(activations)
    encode.short   <- GetShortHash(activations, nProj)
  end_time   <- Sys.time()
  elapse     <-as.numeric(end_time-start_time)
  message(paste("Calculating activations and the binary codes takes" ,sprintf("%.2f",elapse), "seconds."))


  # build a hash table for quick query of short encoded cells
  start_time <- Sys.time()
    library("hash")
    hashset.short  <- hash()
    HashsetInsertMatrix(matrix=encode.short, hash.table=hashset.short)
  end_time   <- Sys.time()
  elapse     <-as.numeric(end_time-start_time)
  message(paste("Hashing short codes takes" ,sprintf("%.2f",elapse), "seconds."))

  # Return a list of embeddings
  message(paste(nrow(data), "data items are processed"))
  return (list("encode.long"  = encode.long  ,
               "encode.short" = encode.short ,
               "activations"  = activations  ,
               "hashset"      = hashset.short))
}


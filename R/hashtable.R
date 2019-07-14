library("hash")

#' Hashtable.key: convert a vector into hashtable key
#'
#' @param vector a numeric vector
#'
#' @return a character variable
#' @export
#'
#' @examples
Hashtable.Key<- function(vector){
  return(paste(vector, collapse = ''))
}

#' HashtableInsertVector: insert a vector element into the hashtable, each slot holds the inserted vector's row index in the matrix
#'
#' @param vector vector to insert
#' @param hash.table a hash table object created using package `hash`
#'
#' @return The function should change the hash table in the parameter and has no output
#' @export
#'
#' @examples
#' # create sample data
#' x <- matrix(
#'     c(1,2,5,1,3,1,
#'       1,5,3,2,4,5,
#'       3,4,2,1,1,0),nrow=3,byrow=TRUE)
#' rownames(x)<- as.character(1:3)
#'
#' # create an empty hashtable
#' h<-hash()
#'
#' # insert the first row into hashtable
#' rowidx<-1
#' h<-HashtableInsertVector(vector=x[rowidx, ], vector.index=rowidx, hash.table=h)
#' h
#' # this should return
#' # <hash> containing 1 key-value pair(s).
#' #  125131 : 1
#'
#' rowidx<-1
#' h<-HashtableInsertVector(vector=x[rowidx, ], vector.index=rowidx, hash.table=h)
#' h
#' # this should return
#' # <hash> containing 1 key-value pair(s).
#' #  125131 : 1 1
#'
#' rowidx<-3
#' h<-HashtableInsertVector(vector=x[rowidx, ], vector.index=rowidx, hash.table=h)
#' h
#' # this should return
#' # <hash> containing 1 key-value pair(s).
#' #  125131 : 1 1
#' #  342110 : 3
HashtableInsertVector <- function(vector, vector.index, hash.table){
  key <- Hashtable.Key(vector)
  if (has.key(key, hash.table)){
    hash.table[[key]] <- c(hash.table[[key]], vector.index)
  }else{
    hash.table[[key]]<- c(vector.index)
  }
  return(hash.table)
}


#' HashtableInsertMatrix: insert a matrix (a set of vector elements, each row is a vector) into the hashtable
#'
#' @param matrix rows of vectors to insert
#' @param hash.table a hash table object created using package `hash`
#'
#' @return a hash table object created using package `hash`
#' @export
#'
#' @examples
#' # create sample data
#' x <- matrix(
#'     c(1,2,5,1,3,1,
#'       1,5,3,2,4,5,
#'       3,4,2,1,1,0),nrow=3,byrow=TRUE)
#' rownames(x)<- as.character(1:3)
#' h<- hash()
#' h<-HashtableInsertMatrix(matrix=x, hash.table=h)
#' h
#' # it should return:
#' <hash> containing 3 key-value pair(s).
#' 125131 : 1
#' 153245 : 2
#' 342110 : 3
HashtableInsertMatrix <- function(matrix, hash.table){
  for (rowid in 1:nrow(matrix)){
    hash.table <- HashtableInsertVector(vector=matrix[rowid,], vector.index=rowid, hash.table=hash.table)
  }
  return(hash.table)
}

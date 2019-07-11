library("hash")


#' HashsetInsert: insert a vector element into the hashset
#'
#' @param vector vector to insert
#' @param hash.table a hash table object created using package `hash`
#'
#' @return The function should change the hash table in the parameter and has no output
#' @export
#'
#' @examples
HashsetInsert <- function(vector, hash.table){
  key <- paste(vector, collapse = '')
  .set(hash.table, keys=key, values=TRUE)
}

#' HashsetInsertMatrix: insert a matrix (a set of vector elements, each row is a vector) into the hashset
#'
#' @param matrix rows of vectors to insert
#' @param hash.table a hash table object created using package `hash`
#'
#' @return a hash table object created using package `hash`
#' @export
#'
#' @examples
HashsetInsertMatrix <- function(matrix, hash.table){
  apply(matrix,1, HashsetInsert, hash.table=hash.table)
  return(hash.table)
}

#' HashsetQuery: query a vector element in the hashset
#'
#' @param vector vector to query
#' @param hash.table a hash table object created using package `hash`
#'
#' @return a boolean variable indicating if the query exists in the hashset
#' @export
#'
#' @examples
HashsetQuery <- function(vector, hash.table){
  key <- paste(vector, collapse = '')
  has.key(vector, hash.table)
}

#' HashsetQuery: query a matrix (a set of vector elements, each row is a vector) in the hashset
#'
#' @param matrix rows of vectors to query
#' @param hash.table a hash table object created using package `hash`
#'
#' @return a boolean vector indicating if each row exists in the hashset
#' @export
#'
#' @examples
HashsetQueryMatrix<- function(matrix, hash.table){
  keys <- apply(matrix, 1, function(row) paste(row, collapse = ''))
  has.key(keys, hash.table)
}

## Functions to create inverse matrix using mean caching

## makeCacheMatrix will create cacheable matrix to be used
## in cacheSolve() function which sets and gets cached values

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x)){
    stop("please provide a matrix")
  }
  
  inverted_matrix <- NULL
  set <- function(y){
    x <<- y
    inverted_matrix <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(solve) inverted_matrix <<- solve
  get_inverse <- function() inverted_matrix
}


## cacheSolve() will compute the inverse of the cacheable matrix
## If the inverse has already been calculated and there is no
## change in the matrix then it returns the cached inverse

cacheSolve <- function(x, ...) {
        inverted_matrix <- x$get_inverse()
        
        if(!is.null(inverted_matrix)){
          message("Got inverted matrix")
          return(inverted_matrix)
        }
        matrix2inverse <- x$get()
        inverted_matrix <- solve(matrix2inverse)
        x$set_inverse(inverted_matrix)
        inverted_matrix
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a "matrix" object and cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  inverse_matrix <- NULL                           
  set <- function(a) {                    
    x <<- a                             ## value of matrix in parent environment
    inverse_matrix <<- NULL             ## when there is a new matrix, reset inverse_matrix to NULL
  }
  get <- function() x                    	## define the get fucntion - returns value of the matrix argument
  setinverse <- function(inverse) inverse_matrix <<- inverse  ## will assign value of inverse_matrix in parent environment
  getinverse <- function() inverse_matrix                     ## gets the value of inverse_matrix when called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}



## Write a short comment describing this function

##This function will perform the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
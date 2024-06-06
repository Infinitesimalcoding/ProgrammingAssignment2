##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

##Write the following functions:
  
  ##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
  ##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##For this assignment, assume that the matrix supplied is always invertible.


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Similar to the example in the Assignment, this function returns a list with 4 elements.
## 1 is a setter to assign a matrix to the list
## 2 is a getter to retrieve the matrix from the list
## 3 is a setter to save the inverse of the matrix
## 4 is a getter to retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##We get the Element saved in the "getInverse" Index of our List
  ##If it exists meaning is.null(m) is not true, we return the cached inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##Else, we have to calculate it. By retrieving the Matrix and inverting it here
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

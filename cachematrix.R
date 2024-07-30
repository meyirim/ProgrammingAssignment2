## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set and get the matrix, as well as to set and
## get the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) inv <<- inverse 
  
  get_inverse <- function() inv
  
  list(set=set, get=get, 
       set_inverse=set_inverse, 
       get_inverse=get_inverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it retrieves the inverse from the cache.
## Otherwise, it calculates the inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  
  inv
}




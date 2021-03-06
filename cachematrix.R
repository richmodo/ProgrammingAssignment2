## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix()) {

##  previous example for set and get of 'mean'  
##  makeVector <- function(x = numeric()) {
##    m <- NULL
##    set <- function(y) {
##      x <<- y
##      m <<- NULL
##    }
##    get <- function() x
##    setmean <- function(mean) m <<- mean
##    getmean <- function() m
##    list(set = set, get = get,
##         setmean = setmean,
##         getmean = getmean)
##  }
  
      m <-NULL
      set <- function(y) {
        x <<- y
        m <<- NULL  
      }
  
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

##  previous 'Cache' example for calculating 'mean'  
##  cachemean <- function(x, ...) {
##    m <- x$getmean()
##    if(!is.null(m)) {
##      message("getting cached data")
##      return(m)
##    }
##    data <- x$get()
##    m <- mean(data, ...)
##    x$setmean(m)
##    m
  
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached inverted matrix")
        return(m)
      }
      data <-x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
  }
  


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  The following function makecachematrix creates a special matrix that can caches its inverse 
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}

get <- function() x
setinverse <- function(solve) m <<-solve
getinverse <- function() m
list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special matrix returned by makeCacheMatrix, if the inverse has already been calculated, then cachesolve will
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) { message("Getting cached data")
     return(m)}
  
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

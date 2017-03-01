## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     
     ## This function creates a special "matrix" object 
     ## that can cache its inverse.   
     
     #store matrix and clear cache
     cachematrix <- NULL
     set <- function(y) {
          x <<- y
          cachematrix <<- NULL
     }
     
     #returns matrix which is stored
     get <- function() x
     
     #cache the inverse matrix
     setinverse<- function(solve) cachematrix <<- solve
     
     #get the inverse matrix
     getinverse <- function() cachematrix
     
     #returns list
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## This function computes the inverse of the special "matrix" 
     ## returned by makeCacheMatrix above. If the inverse has already 
     ## been calculated (and the matrix has not changed), then the 
     ## cachesolve should retrieve the inverse from the cache.
     
     ## get the cached matrix
     cachematrix <- x$getinverse()
     
     ## Checks if there's a value, if so, then give that matrix
     if(!is.null(cachematrix)) {
          message("getting cached data")
          return(cachematrix)
     }
     
     ## If no matrix is cached, calculate the inverse
     data <- x$get()
     cachematrix <- solve(data, ...)
     x$setinverse(cachematrix)
     
     ## Print inverse matrix
     cachematrix
}

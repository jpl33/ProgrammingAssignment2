## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makecahcematrix is a function that creates a matrix that saves its inverse  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## Write a short comment describing this function
#cachesolve receives a parameter matrix , checks if its inverse is cached.
# if it is - cachesolve returns the cached value
#if it isn't - it computes the inverse, saves it, and returns it 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    #return the cached inverse matrix
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmean(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
 
}

## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(solve) m <<- solve
  getcache <- function()mlist(set = set, get = get,
                              setcache = setcache,
                              getcache = getcache)
  
}


## This function calculates or solve the inverse of a matrix

cacheSolve <- function(x, ...) {
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}

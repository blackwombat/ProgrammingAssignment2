## Put comments here that give an overall description of what your
## functions do

##Creates a wrapper for a matrix, which will store cached inverses
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL #inverse cache, stored in local environment, accessible from wrapper functions, but not from outside
    
    #sets matrix inside wrapper, clears cached inverse (<< is used to access parent environment)
    set <- function(y) {
      x <<- y
      cache <<- NULL
    }
    
    #returns unwrapped matrix
    get <- function() x
    
    #put inverse into cache
    setSolve <- function(solve) cache <<- solve
    
    #returs inverse from cache
    getSolve <- function() cache
    
    #sets wrapper api and returns wrapper
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Calculates matrix inverse and sets in on wrapper, or returns cached result if present
cacheSolve <- function(x, ...) {
  m <- x$getSolve()# Looking if we have cached result ...
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # ... if so, return it
  }
  data <- x$get() #get unwrapped matrix
  m <- solve(data, ...) # calculate inverse
  x$setSolve(m) # put results into wrapper cache
  m #return result
}

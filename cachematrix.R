## makeCacheMatrix and cacheSolve work together to return the inverse of a Matrix, either by checking the cache 
## or if it was not cached yet, by calculating the inverse


## This function creates a special vector of a given matrix that needs inverting with functions 
## to handle matrix caching, by allowing to set and get the matrix and by allowing to set  
## and get the inverse, regardless of the environment.
makeCacheMatrix <- function(x = matrix(1:1)) { # x is a matrix, but matrix needs at least the data element, so I put a default
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes the special vector produced by makeChacheMatrix and gets the inverse. If that is not existing, 
## it calculates the inverse and stores it using the setinverse function of makeChacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                         
  if(!is.null(inv)) {                           #check if inverse was cached
    message("getting cached data")
    return(inv) 
  }
  data <- x$get()
  inv <- solve(data, diag(dim(data)[1]), ...)   #diag(dim(data)[1]) creates unity matrix with same dimension as data, forcing "solve" to revert the inverse
  x$setinverse(inv)
  inv
}

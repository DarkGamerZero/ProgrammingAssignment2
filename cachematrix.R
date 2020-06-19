## This function attempts to speed up the matrix inversion option by pre-caching
## the inverse of the matrix. If the inverse exists, it is fetched from the cache else 
## it is calculated and saved to the cache for easy retreival


## The first function makeCacheMatrix() just creates a special matrix in order to store the calculated inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The second function cacheSolve() calculates the mean of the matrix made using makeCacheMatrix function
## First, it checks if the inverse already exists in cache. If yes, fetch inverse from cache and skip calaulation
## If no, calculate inverse of input matrix and save it to cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
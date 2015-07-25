## this set of functions creates a r object that contains a matrix,
## and functions that store, retrieve, and compute its inverse.

## this function returns a list of storage and retrevial functions and
## stores a matrix and its inverse inside the function enviroment.
## for its argument, submit a square, invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #create the inv object within the function enviroment
  
  set <- function(y) { #function for changing the matrix and resetting the inverse
    x <<- y
    inv <<- NULL
  }
  get <- function() x #returns the stored matrix
  setinverse <- function(inverse) inv <<- inverse #stores the inverse in the parent enviroment
  getinverse <- function() inv #returns the stored inverse
  list(set = set, get = get, #returns the full set of functions to parent enviroment
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function retrieves a matrix inverse from the cache or computes it
## for the argument, submit an object created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ##reads inverse from makeMatrix() enviroment
  if(!is.null(inv)) { ##return inverse from makeMatrix() if already cached
    message("getting cached data")
    return(inv)
  }
  data <- x$get()##read matrix from makeMatrix()
  inv <- solve(data, ...)##compute inverse
  x$setinverse(inv)##store inverse in makeMatrix() enviroment
  inv ## Return a matrix that is the inverse of 'x'
}

##I have copied the repository to my local machine and opened it directly
##there so that whenever I save the file, I do not have to setwd

## The overall description of my functions are to cache the inverse of a matrix

## x = matrix to make it easier to follow what we are solving for
## inv serves as NULL which is the what we are solving for

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
      }
  
  #this is to get the matrix (remember x = matrix)
  get <- function() x
  
  #this is to set the inverse of the matrix and set it to inv
  setinverse <- function(inverse) inv <<- inverse
  
  #this is to get the inverse of the matrix whicch is now inv
  getinverse <- function() inv
  
  #gives us the list of all the functions above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this computes for the inverse calculated from makeCacheMatrix function above
## as mentioned in the assignment, if the inverse is already calculated
## and there is no change in the matrix, this functions just retrieves it from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ##this is for retrieving cached inverse if it is already computer from before
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
  
  ##this is for computing and retrieving the inverse if it is not computed yet
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}


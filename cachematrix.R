## Put comments here that give an overall description of what your
## functions do

## This function takes in a matrix, and uses the cachesolve function 
## to get the inverse of the matrix.  It returns a list of functions
## that sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse matrix, and gets the value of the inverse matrix
## All of this is stored in memory, which may save time

## default value is an empty matrix
makeCacheMatrix <- function(x = matrix()) {
  # i is initally set to null
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## This function computes the inverse of the matrix created in the 
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ## If the i variable is not null, it returns i 
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  ## uses get function from makeCacheMatrix to return x
  matrixvariable <- x$get()
  ## gets the inverse of x and puts it in variable i
  i <- solve(matrixvariable)
  x$setinverse(i)
  i
}




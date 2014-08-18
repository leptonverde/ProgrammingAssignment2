## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##Make inv a null value for future use
  setInverse <- function(i){ ##Function to Set the Inver value of the Matrix
    inv <<- i
  }
  getInverse <- function() inv  ##return the inv Value
  set <- function(val){  ## Asign val to x, makes inv equal to Null to recalculate the inverse
    inv <<- NULL
    x <<- val
  }
  get <- function() x ## return x
  
  ##Build the Special Matrix as a List of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){ ## Inverse Already calculated, lets return the cached value
    return(inv)
  }
  
  inv <- solve(x$get(), ...) ##Inv is Null, so is time to calculate the inverse
  x$setInverse(inv)
  inv
}

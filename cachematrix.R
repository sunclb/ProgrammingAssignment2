## This function provide functions to get, set matrix and get and set the inverse 
## of the matrix



makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  ## Set matrix function
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  ## get matrix function
  get <- function() x
  ## set the inverse of the matrix function
  setinverse <- function(inverse) iv <<- inverse
  ## get the inverse of the matrix function
  getinverse <- function() iv
  ## return functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function check whether the inverse of matrix is avaliable, if so, get and
## return it, and if not, calculate the inverse, store it and return it

cacheSolve <- function(x, ...) {
  ## Check whether the inverse of matrix is avaliable. If so, return the inverse
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  ## When inverse of matrix is not avaliable, calculate the inverse, store it and return it
  data <- x$get()
  iv <- solve(data)
  x$setinverse(iv)
  iv
  }

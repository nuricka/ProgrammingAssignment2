################################################################################
#                                                                              #
#                        makeCacheMatrix - cacheSolve                          #
#                                                                              #
#    Functions that is able to cache the inverse of an invertible matrix       #
#    (the matrix supplied is always invertible) to avoid repeating the         #
#    time-consuming computation of calculating the inverse.                    #
#                                                                              #
################################################################################

## makeCacheMatrix function creates a special "matrix" object that can cache:
#  * its inverse to avoid recalculating in the future.
#  * the matrix itself to determine whether the matrix has changed

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  setmat <- function(z) mat <<- z
  getmat <- function() mat
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv,
       setmat = setmat, getmat = getmat)  
}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
# only when necessary

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  m <- x$get()
  if(!is.null(inv))  # does the inverse exist?
  {
    mat<-x$getmat()
    if (identical(m,mat)) # has the matrix changed?
    {
      message("getting cached data to restore the inverse of the matrix")
      return(inv)
    }
  }
  #data <- x$get()
  inv <- solve(m, ...)
  # store the new values for matrix and inverse
  x$set(m) 
  x$setmat(m)
  x$setinv(inv) 
  inv
  
}

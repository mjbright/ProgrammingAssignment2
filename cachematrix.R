
################################################################
#
# cachematrix: The two functions defined below are to be used
#    to provide cached matrix inversion functionality.
#
# Usage:
#    Given a square matrix, e.g. mymatrix below, we create
#    a special 'cached matrix', called mycachedmatrix here
#    using the function makeCacheMatrix.
#
#    Calling cacheSolve a first time on mycachedmatrix
#    will calculate the inverse matrix, stock it for
#    future use and return the inversed matrix.
#
#    Calling cacheSolve a second time on mycachedmatrix
#    will returned the already cached inverse matrixvalue.
#
# e.g.
# First create the initial matrix:
# > mymatrix <- matrix(rnorm(16), 4, 4)
# > mymatrix
# [,1]       [,2]       [,3]       [,4]
# [1,] -0.02881534  0.6552276  2.1377671  0.5328970
# [2,]  0.23252515 -0.4006375  0.5058193 -0.6458943
# [3,] -0.30120868 -0.3345566  0.7863424  0.2909875
# [4,] -0.67761458  1.3679540 -0.9022119 -1.2375945
#
# Then create the 'cached matrix':
# > mycachedmatrix <- makeCacheMatrix(mymatrix)
#
# Call cacheSolve a first time on mycachedmatrix:
# > cacheSolve(mycachedmatrix)
# [,1]       [,2]        [,3]         [,4]
# [1,] 0.37156802  0.4365295 -1.87444411 -0.508554475
# [2,] 0.46573502 -0.3680944 -0.79272594  0.206259444
# [3,] 0.30848489  0.3418263  0.22033377  0.006239046
# [4,] 0.08646241 -0.8950709 -0.01054439 -0.306135492
#
# Call cacheSolve a second time on mycachedmatrix:
# > cacheSolve(mycachedmatrix)
# getting cached 'inverse matrix' data
# [,1]       [,2]        [,3]         [,4]
# [1,] 0.37156802  0.4365295 -1.87444411 -0.508554475
# [2,] 0.46573502 -0.3680944 -0.79272594  0.206259444
# [3,] 0.30848489  0.3418263  0.22033377  0.006239046
# [4,] 0.08646241 -0.8950709 -0.01054439 -0.306135492

  
################################################################
#
# function makeCacheMatrix
#
# creates a special "matrix" object that can cache its inverse value.
#
makeCacheMatrix <- function(x = matrix()) {
  # inverse: the cached value which is initially set to NULL
  #          and reinitialized to NULL anytime the matrix is "set"
  inverse <- NULL
  
  # set: This function sets a new matrix value and resets the cached value
  set <- function(y) {
      x <<- y
      inverse <<- NULL
  }
  
  # get: just returns the matrix values
  get <- function() x
  
  # setinverse: sets the cached value using the supplied inv argument
  setinverse <- function(inv) inverse <<- inv
  
  # getinverse: gets the cached value
  getinverse <- function() inverse
  
  # Return a list containing the 4 functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


################################################################
#
# function cacheSolve
#
# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
#
cacheSolve <- function(x, ...) {
    ## Get the currently stocked 'inverse' value
    inverse <- x$getinverse()
  
    # If the 'inverse' value is set (not NULL) we can just return it here
    if(!is.null(inverse)) {
        message("getting cached 'inverse matrix' data")
        return(inverse)
    }
    
    # 'inverse' value is not set so we must calculate it:
    
    # Use solve to calculate the inverse of the matrix values returned by x$get()
    inverse <- solve( x$get(), ...)
    
    # Set the cached value to 'inverse'
    x$setinverse(inverse)
    
    # Return the newly calculated 'inverse'
    inverse
}

################################################################
#
# function demonstrateCacheSolve
#
# demonstrateCacheSolve demonstrates the use of functions
#    - makeCacheMatrix
#    - cacheSolve
#
demonstrateCacheSolve <- function() {
  print("demonstrateCacheSolve:")
  mymatrix <- matrix(rnorm(16), 4, 4)
  
  print("Created random 4x4 matrix:")
  write.table(mymatrix)
  
  mycachedmatrix <- makeCacheMatrix(mymatrix)
  print("Created cached matrix of random 4x4 matrix:")
  write.table(mycachedmatrix$get())
  
  inv <- cacheSolve(mycachedmatrix)
  print("Call cacheSolve 1st time:")
  write.table(inv)
  
  inv <- cacheSolve(mycachedmatrix)
  print("Call cacheSolve 2nd time:")
  write.table(inv)
}

demonstrateCacheSolve()



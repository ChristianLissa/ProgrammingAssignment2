## This function will can cache the inversed matrix of the input matrix

makeCacheMatrix <- function(mX = matrix()) {
  
  ## Cache for inversed matrix
  mInverse <- NULL
  
  set <- function(y) {
    ## Copy
    mX <<- y
    ##Init
    mInverse <<- NULL
  }
  ## Return parameter
  get <- function() mX
  
  ## Use R.solve on mInverse
  setinverse <- function(solve) mInverse <<- solve
  
  ## Return mInverse, if present, ...
  getinverse <- function() mInverse
  
  ## Name it
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function can calculate the an inverted matrix, if it is not already present

cacheSolve <- function(mX, ...) {
  
  ## Catch inverse matrix if present, ...
  mResult <- mX$getinverse()
  if(!is.null(mResult)) {
    message("Inverse matrix already calculated")
    return(mResult)
  }
  ## else get matrix
  mOrig <- mX$get()
  ## Calculate inverse matrix
  mResult <- solve(mOrig, ...)
  ## Cache inverse Matrix
  mX$setinverse(mResult)
  ## Print
  mResult
}


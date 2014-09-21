## Second programming assignment
## R Programming | Coursera
## September 2014

## Matricx inversion is usually a costly computation. If the contents of a matrix is not changing 
## it may make sense to cache the value of the inverse matrix so that when we need it again, it 
## can be looked up in the cache rather than recomputed. 

## In this module there is a pair of functions (makeCacheMatrix, cacheSolve) that cache the inverse 
## of a matrix.

## To test
## Create a sample matrix
##    x = matrix(c(2, 5, 4, 3, 2, 1, 5, 9, 7), nrow=3, ncol=3)
## Call the first function makeCacheMatrix with this matrix
##    m = makeCacheMatrix(x)
## To make sure that the sample matrix was set call the get function
##    m$get()
## which should return the matrix
##    > m$get()
##    [,1] [,2] [,3]
##    [1,]    2    3    5
##    [2,]    5    2    9
##    [3,]    4    1    7
## Now call the second function to return the inverse matrix
## Note that the first time the inverse is computed.
##    > cacheSolve(m)
##    [,1] [,2] [,3]
##    [1,] -2.5    8 -8.5
##    [2,] -0.5    3 -3.5
##    [3,]  1.5   -5  5.5
## However, the second time the inverse is returned from the cache.
##    > cacheSolve(m)
##    getting cached data
##    [,1] [,2] [,3]
##    [1,] -2.5    8 -8.5
##    [2,] -0.5    3 -3.5
##    [3,]  1.5   -5  5.5

###############################################################################################################
## Function to set/get the value of the matrix and to set/get the value of the inverse matrix
###############
makeCacheMatrix <- function(x = matrix()) {
  
  ## cached inverse matrix
  m <- NULL
  
  ## set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## set the inverse matrix
  setinverse <- function(solve) m <<- solve
  
  ## get the inverse matrix
  getinverse <- function() m
  
  ## list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


###############################################################################################################
## Function to compute the matrix inverse. 
## First the matrix inverse is checked to see if it has already been calculated. 
## If so, the function simply returns the inverse of the matrix from the cache and skips the computation. 
## Otherwise, the matrix inverse is computed and saved in the cache via the setinverse function.
##########
cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix,
  data <- x$get()
  ## compute its inverse, and assign it to the cache variable
  m <- solve(data, ...)
  
  x$setinverse(m)

  ## Return a matrix that is the inverse of 'x'
  m
  
}

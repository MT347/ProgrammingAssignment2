## Put comments here that give an overall description of what your
## functions do

## together, the two functions make working with matrices more efficient
## because a matrix can be stored together with its inverse.
## The inverse only needs to be computed the first time it is called.
## For all subsequent calls the stored inverse can be use.

## Write a short comment describing this function

## The function 'makeCacheMatrix' takes a matrix as its argument.
## The function then creates an empty vector that will later be used to store 
## (to cache) this matrix' inverse.
## It also creates 4 functions (see comments in the code for details).
## Finally these 4 functions are returend by 'makeCacheMatrix' as a list of length 4.
## Because the 4 functions in the list have been created by the function 'makeCacheMatrix'
## they will have access to the data environment of 'makeCacheMatrix' whenever
## they are called (lexical scoping).


makeCacheMatrix <- function(x = matrix(nrow = 0, ncol = 0)) {
  
  ##initialize chache
  i <- NULL
  
  ## The function 'set' changes the matrix that is stored in the environment of
  ## 'makeCacheMatrix'
  ## 'set' also eareases (resets) the inverse matrix stored in the environment 
  ## of 'makeCacheMatrix'
  set <- function(y){
    ##store new matrix
    x <<- y
    ##reset inverse
    i <<- NULL
  }
  
  ## The function 'get' retrieves the matrix currently stored in the environment
  ## of 'makeCacheMatrix'
  get <- function() x
  
  ## The function 'setinverse' stores the matrix it gets as its argument in the 
  ## environment of 'makeCacheMatrix'.
  ## This will be used to store (to cache) the inverse of the matrix.
  setinverse <- function(inverse) i <<- inverse
  
  
  ## The function 'getinverse' retrieves the matrix' inverse that is stored in 
  ## the environment of 'makeCacheMatrix'
  getinverse <- function() i
  
  
  ##return the 4 function (objects) in a list to make them available 
  ## for other objects
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function

## In combination with the function 'makeCacheMatrix' the function 'cacheSolve' 
## can be used like the R function 'solve' to compute the inverse of a matrix.
## Unlike 'solve' however, the function 'cacheSolve' will first check if the inverse
## has already been computed and stored.
## Only if this is not the case the inverse will be computed.
## Otherwise the stored version will be used.

cacheSolve <- function(x) {
  
  ##read the cache
  i <- x$getinverse()
  
  ##check if the cache contains a value
  if(!is.null(i)) {
    #if a value is found, it will be returned; no further calculation is needed
    message("getting cached data")
    ##return the cached inverse and exit function
    return(i)
  }
  
  ## this section will only be used if the cache is empty:
  ## retrieve original matrix
  data <- x$get()
  ## compute the matrix' inverse
  i <- solve(data)
  ## write the inverse to the cache in case it will be called again later
  x$setinverse(i)
  
  # return the (newly computed) inverse
  message("inverse calculated and stored in cache")
  i
  
}

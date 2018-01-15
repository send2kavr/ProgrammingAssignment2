## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function will create a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL 
  
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
 getMatrix <- function()x
   setInverse <- function(inverse)  
     invMatrix <<- inverse
 getInverse <- function() invMatrix

   list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, 
        getInverse = getInverse)   

}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {   
    message("Getting Cached Invertible Matrix")
    return(invMatrix)
  }
  
  MatrixData <- x$getMatrix() 
  invMatrix <- solve(MatrixData, ...) 
  x$setInverse(invMatrix)
  return(invMatrix) 
}


### Testing #####

TM<-matrix(1:6, 2,3)
TM
CacheMatrix  <- makeCacheMatrix(TM)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()
CacheMatrix$setMatrix(TM)
CacheMatrix$setInverse(TM)



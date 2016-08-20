## Put comments here that give an overall description of what your
## functions do

## ============================================================================
## The Assignment: write a pair of functions that cache the inverse of a matrix.
## ============================================================================

## Write the following functions:
## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the `solve`
## function in R. For example, if `X` is a square invertible matrix, then
## `solve(X)` returns its inverse.

## NOTE: For this assignment, assume that the matrix supplied is always invertible.

## From the project assignment, the following examples can be used:
## makeVector <- function(x = numeric()) {
##   m <- NULL
##   set <- function(y) {
##     x <<- y
##     m <<- NULL
##   }
##   get <- function() x
##   setmean <- function(mean) m <<- mean
##   getmean <- function() m
##   list(set = set, get = get,
##        setmean = setmean,
##        getmean = getmean)
## }
## cachemean <- function(x, ...) {
##   m <- x$getmean()
##   if(!is.null(m)) {
##     message("getting cached data")
##     return(m)
##   }
##   data <- x$get()
##   m <- mean(data, ...)
##   x$setmean(m)
##   m
## }

## ============================================================================
## The Sollution:
## ============================================================================

## Description:
## The function 'makeCacheMatrix' creates a special "matrix" that can cache its inverse.
## Input:
## x: a square invertible matrix
## Output:
## The function returns a list containing 4 functions:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # declare variables:
  x.inverse <- NULL

  # define the functions:
  set <- function(y) {
    # The '<<-' operator can be used to assign a value to an object in an environment
    # that is different from the current environment.
    x <<- y
    x.inverse <<- NULL
  }
  get <- function() x
  setInverseOfInput <- function(solve) x.inverse <<- solve
  getInverseOfInput <- function() x.inverse

  # return the result:
  list(
    set=set,
    get=get,
    setInverseOfInput=setInverseOfInput,
    getInverseOfInput=getInverseOfInput
  )
}

## Description:
## The function 'cacheSolve' calculates the inverse of the special "matrix"
## created with the function 'makeCacheMatrix'. However, it first checks to see
## if the inverse has already been calculated. If so, it get's the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the
## 'setInversOfInput' function.
## Input:
## x: a square invertible matrix
## Output:
## The function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # get the inverse from the cache
  x.inverse <- x$getInverseOfInput()

  # if the inverse already has been calculated
  # return the inverse retrieved from the cache
  if(!is.null(x.inverse)){
    message("getting cached data")
    return(x.inverse)
  }

  # the inverse has not been calculated yet
  data <- x$get()
  x.inverse <- solve(data, ...)

  # save the inverse to the cache
  x$setInverseOfInput(x.inverse)

  # return the result
  x.inverse
}

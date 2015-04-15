# File:   cachematrix.R
# Date:   2015-04-15
# Author: Rick Wargo
#
# Description
#   Assuming a square, invertable matrix, calculate the inverse of that matrix. 
#   Due to the high cost of calculating the inverse of a matrix, save on processing time by 
#   returning a cached copy of the matrix if the inverse has been previously calculated.
#
# Modification History
# RCW  2015-04-15  New today


makeCacheMatrix <- function(square.matrix) {
  # Create a CacheMatrix object that can cache the inverse of a squared matrix.
  #
  # Args:
  #   square.matrix: must be a square, invertible matrix (assumed to be square, no test for it)
  #
  # Returns:
  #   list of four functions to operate on the cached matrix
  
  inverse.of.matrix <- NULL
  
  # setters/getters for the input matrix
  set <- function(a.matrix) {
    square.matrix <<- a.matrix
    inverse.of.matrix <<- NULL  # remove the caced copy if the input matrix changes
    
    # TODO: check if input matrix is identical to current matrix and if so, no need to clear cache
  }
  get <- function() square.matrix
  
  # setters/getters for the inverse matrix
  setInverse <- function(a.inverse) inverse.of.matrix <<- a.inverse
  getInverse <- function() inverse.of.matrix
  
  # return a vector of four methods to operate on the matrix and its inverse
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(objCacheMatrix) {
  # Returns a matrix that is the inverse of given matrix.
  #
  # Args:
  #   objCacheMatrix: initialized CacheMatrix object
  #
  # Returns:
  #   inverse matrix of the given matrix.
  
  inverse.of.matrix <- objCacheMatrix$getInverse()
  if(!is.null(inverse.of.matrix)) {
    # If the inverse has been previously calculated, return the cached copy
    #message("getting cached data")
    return(inverse.of.matrix)
  }
  
  # calculate the inverse of the matrix (first time) and cache the result
  matrix <- objCacheMatrix$get()
  inverse.of.matrix <- solve(matrix)
  objCacheMatrix$setInverse(inverse.of.matrix)
  
  # return matrix inverse
  inverse.of.matrix
}

######################### Testing Harness #########################
#
# To use, the RUnit CRAN package must be installed.
#   install.packages("RUnit")
#
library("RUnit")
source("test_helpers.R")

test.makeCacheMatrix.returns.length.4 <- function() {
  # Test to determine if the makeCacheMatrix function returns a vector of length 4
  #
  # Args:
  #   none
  #
  # Returns:
  #   Generates an error if any test fails

  test.matrix = hilbertMatrix(8)
  
  cache.matrix.obj = makeCacheMatrix(test.matrix)
  checkTrue(length(cache.matrix.obj) == 4)
}

test.can.calculte.inverse <- function() {
  # Test to determine if the cacheSolve function returns the inverse of a specified matrix
  #
  # Args:
  #   none
  #
  # Returns:
  #   Generates an error if any test fails

  test.matrix = hilbertMatrix(8)
  
  cache.matrix.obj = makeCacheMatrix(test.matrix)
  inverse = cacheSolve(cache.matrix.obj)
  
  # matrix * inverse(matrix) --> identity matrix
  checkTrue(isIdentity(round(test.matrix %*% inverse)))
}

test.is.inversed.cached <- function() {
  # Test to determine if the inverse matrix is cached after calling cacheSolve
  #
  # Args:
  #   none
  #
  # Returns:
  #   Generates an error if any test fails

  test.matrix = hilbertMatrix(8)
  
  cache.matrix.obj = makeCacheMatrix(test.matrix)
  
  checkTrue(is.null(cache.matrix.obj$getInverse()))   # inverse should be NULL prior to cacheSolve
  identity = cacheSolve(cache.matrix.obj)  
  checkTrue(!is.null(cache.matrix.obj$getInverse()))  # inverse should NOT be NULL after cacheSolve
}

perform.unit.tests <- function() {
  # Perform series of unit tests to check implementation
  #
  # Args:
  #   none
  #
  # Returns:
  #   Returns true without any error messages if all tests pass
  #   Generates an error if any test fails
  
  test.makeCacheMatrix.returns.length.4()
  test.can.calculte.inverse()  
  test.is.inversed.cached()
  
  return(TRUE)
}

# Each time this file is loaded, the unit tests will be run to ensure implementation matches design
perform.unit.tests()

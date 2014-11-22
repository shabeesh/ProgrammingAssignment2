## This file contain the code as a solution for Programming Assignment 2 for the Coursera Course on R Programming.
## The objective of the program is to
##    (a) define a makeCacheMatrix function which will return an object with the matrix and its inverse.
##    (b) define a cacheSolve() function which takes the object created by makeCacheMatrix(x) function and return the inverse of the matrix.
## If the inverse is computed then, return the cached inverse.
## 
## Assumption : The matrix provided is always invertible. 
##
## This code passed the following test
## x <- c(1,3)
## y <- c(2,4)
## m <- cbind(x,y) 
## x1 <- c(4,3)
## y1 <- c(3,2)
## m1 <- cbind(x1,y1)
## source('cachematrix.R')
## obj <-makeCacheMatrix(m)
## cacheSolve(obj) - returned the inverse of matrix dentoed by m
## cacheSolve(obj) - returned the inverse and also printed the message the value is returned from the cache.
## obj$set(m1)
## cacheSolve(obj) - returned the inverse for matrix denoted by m1
## cacheSolve(obj) - returned the inverse and also printed the message the value is returned from the cache.



## makeCacheMatrix function creates a special object that stores the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  currInv <- NULL             ## set the inv as null
  currMatrix <- x             ## set the matrixVal attribute equal to the value x 
  
  ## set is the function that sets the matrix into the object.
  set <- function(data){      
    currMatrix <<- data
    currInv <<- NULL          ## As a new matrix is set to currMatrix, the currInv   
  }

  ## get is the function that returns the current matrix set int the special matrix object.
  get <- function() currMatrix
  
  ## set the inverse of the matrix
  setInv <- function(inverse) currInv <<- inverse
  
  ## get the inverse of the matrix
  getInv <- function() currInv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The function takes the special matrix object created out by the makeCacheMatrix function and returns the inverse of the matrix set
## in the object. If the inverse is set, then it returns the cached inverse else it recomputes the inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$getInv() 
    
    if(!is.null(inverse)) {
      message("returning cached data")
      return(inverse)        
    }
    
    currMatrix <- x$get()
    
    y <- solve(currMatrix)
    
    x$setInv(y)
    
    y
    
}
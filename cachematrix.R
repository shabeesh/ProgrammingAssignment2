## This file contains the code as a solution for Programming Assignment 2 for the Coursera Course on R Programming.
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
  currInv <- NULL             ## currInv, holds the inverse of the matrix, is initialised to null
  currMatrix <- x             ## currMatrix, holds the current matrix, is intialised to x
  
  ## set is the function that sets the data passed as argument into currMatrix value.
  ## As a new matrix is set to currMatrix, the currInv is set to NULL so that it can be recomputed by cacheSolve
  set <- function(data){      
    currMatrix <<- data
    currInv <<- NULL          
  }

  ## get is the function that returns the current matrix.
  get <- function() currMatrix
  
  ## sets the inverse of the matrix
  setInv <- function(inverse) currInv <<- inverse
  
  ## gets the inverse of the matrix
  getInv <- function() currInv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The function takes the special matrix object created by the makeCacheMatrix function and returns the inverse of the matrix 
## in the object. If the inverse is set, then it returns the cached inverse else it recomputes the inverse and sets it into the object.

cacheSolve <- function(x, ...) {

  ## obtain the inverse from the object passed as argument. Note that if the object is newly created or if the set function is called the 
  ## inverse returned wil be null.
  inverse <- x$getInv() 
  
  ## If inverse is not null, it means the inverse was already calculated, hence return is possible.
  if(!is.null(inverse)) {
      message("returning cached data")
      return(inverse)        
  }
   
  currMatrix <- x$get()     ## if not get the matrix from the object
  y <- solve(currMatrix)    ## calculate the inverse.
    
  x$setInv(y)               ## set the inverse
  y                         ## return the inverse
}
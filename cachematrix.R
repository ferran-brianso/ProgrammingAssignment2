####################################################################
## Code corresponding to Coursera's R Programming Week 3 Assignment
####################################################################
##
## Author: ferran.brianso@gmail.com
## Last modified: 2015/05/23
##
####################################################################
## Functions:
##  - makeCacheMatrix, that creates a special matrix object with 
##      methods to set and get the matrix and its inverse
##  - cacheSolve, that gets the inverse matrix of x if it has been
##      calculated, otherwise it calculates and sets its value
####################################################################


##################################################
############# makeCacheMatrix ####################
### Creates a special "matrix", which is in fact 
### a list containing functions to:
###   1. set the values of the matrix
###   2. get the values of the matrix
###   3. set the values of the inverse matrix
###   4. get the values of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m){
    x <<- m
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##################################################
##################################################


##################################################
################ cacheSolve ######################
### Calculates the inverse of the given matrix x, 
### created with the makeCacheMatrix function.
### However, it first checks to see if the inverse 
### has already been calculated. 
### If so, it gets the inverse matrix from the 
### cache and skips the computation. 
### Otherwise, it calculates the inverse of the 
### matrix using solve() function and sets its 
### value in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinv(inv)
  inv
}
##################################################
##################################################

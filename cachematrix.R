## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a list containing a function that does 4 things
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse of the matrix
## 4. Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of the matrix using the following method
## The assumption if that the function assumes that the matrix can be inversed
## The function checks to determine if the inverse of the matrix was computed
## If the inverse of the matrix was computed the function gets the result
## and no computation occurs (i.e. it skips the computation step)
## If the function determines the inverse was not computed then
## the function calculates/computes the inverse matrix and 
## sets the value in the cache through the setinverse function

cacheSolve <- function(x, ...) {
   inv <-x$getinverse()
   if(!is.null (inv)) {
     message("getting cached data.")
     return (inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}

## I tested the functions to see if they worked :)
## > source('~/Documents/PROFDEV/COURSERA/DataScience/R-Programming/Assignments/ProgrammingAssignment2/cachematrix.R')
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > y = makeCacheMatrix(x)
## > y$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > 
 ## > ## No cache in the initial run
 ## > cacheSolve(y)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(y)
## getting cached data.
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
 
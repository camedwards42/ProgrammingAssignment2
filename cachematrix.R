## makeCacheMatrix takes in a function variable of type matrix that is invertible 
     ## and creates a list object that contains a set, get, setinv, getinv, and 
     ## environment data that can point to the appropriate functions

## cacheSolve takes in a function variable that is of type list that is a direct 
     ## output from the makeCacheMatrix function. cacheSolve will solve the 
     ## inverse of the matrix and store it in cache if not already done so

## myMat <- makeCacheMatrix(aMat)
## cacheSolve(myMat)
## inv <- myMat$getInv()

## Write a short comment describing this function

## makeCacheMatrix sets the invMat to null (resets every time this function is called)
     ##, creates the setter and getter functions that get set in the output list
     ## this function holds the environment and appropriate pointers to the cached
     ## inverted matrix

makeCacheMatrix <- function(x = matrix()) {
     invMat <- NULL
     set <- function(y) {
          x <<- y
          invMat <<- NULL
     }
     get <- function() x
     setInv <- function(solve) invMat <<- solve
     getInv <- function() invMat
     list(set = set, get = get, 
          setInv = setInv,
          getInv = getInv)
}


## cacheSolve takes the output from makeCacheMatrix and solves for the inverse of
     ## the input matrix if it is solvable. 
     ## Code first checks to verify that the matrix has not already been cached,
     ## if it has been cached, it retrieves it
     ## If it has not been cached then it gets the input matrix and sovles for 
     ## the inverse. The function will then return the inverse matrix

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     invMat <- x$getInv()
     if (!is.null(invMat)) {
          message("Getting cached data")
          return(invMat)
     }
     data <- x$get()
     invMat <-solve(data,...)
     x$setInv(invMat)
     return (invMat)
}

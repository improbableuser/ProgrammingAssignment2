## Repeatedly calculating the inverse of matrices is time consuming and computationally 
## inefficient. Creating an efficient way to reference and use a previously calculated
## inverse can help speed up computation.


## This function creates a new environment, where the inverse of a matrix will be 
## stored. This function utilizes R's lexical scoping to store the calculated 
## inverse matrix within a list. This list is then reference when the inverse
## matrix is needed for calculations.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x 
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function uses base R solve function to calculate the inverse of the given
## matrix. Then stores the result in the list made by the makeCacheMatrix function.
## Additionally, the cacheSolve function checks if the inverse has previously been
## calculated and stored. If that is the case, then a message informs the user that
## the inverse matrix has been cached and it will not be recalculated. If the 
## inverse matrix has not been cached, then it will solve for the inverse and
## cache it in the environment created by the makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("Matrix equal, using cached data")
                return(m)
        }
        else {
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          return(m)
        }
}

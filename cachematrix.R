## These functions create a special type of matrix, which stores the value of its inverse
## this is time-saving when the matrix values don't change, 
## and you're trying to get the inverse repeatedly (like in a long loop), 
## or when the matrix is very large, and computing the inverse again even once more is painfully long

## Creates the special type of matrix, the cache matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function(inverse) inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## when called, this function outputs the inverse of the matrix.
## if called for the first time, computes and stores the value of the inverse
## if called another time, does not compute and simply gets the value stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

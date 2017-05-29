## Put comments here that give an overall description of what your
## functions do

## Create a list of functions tied to a specific matrix for use in future functions.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL           #creates inverse object, sets it to NULL
    set <- function(y){
        x <<- y         #resets value of the matrix object
        m <<- NULL      #if set fxn is used, m is reset to NULL
    }
    get <- function() x #returns x object
    setinverse <- function(inverse) m <<- inverse #m is defined in second function; it is the inverse of the matrix, or NULL if it has not been cached or is reset
    getinverse <- function() m                    #returns the value of m
    list(set = set,                               #creates a list with each fxn as a named object
         get = get, setinverse = setinverse,
         getinverse=getinverse)
}


## Cache the result of the inverse of a matrix 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}

#test it out...
B = matrix( 
    c(2, 4, 3, 1), 
    nrow=2, 
    ncol=2)
solve(B)
test2 <- makeCacheMatrix(matrix(c(5,5,5,5), nrow=2, ncol=2))
test2$get()
test2$getinverse()
test2$set(B)
test2$get()
cacheSolve(test2)
test2$getinverse()

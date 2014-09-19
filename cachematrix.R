## makeCacheMatrix and CacheSolve together  
## allows the inverse of matrix x to be cached.

## makeCacheMatrix: input is a square matrix
## the function provides a list of access functions
## set, get, setinverse and getinverse to input x
## Every time the function runs, it initializes the
## inverse matrix of x
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(z) inv <<- z
        
    getinverse <- function() inv
    list (set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## cacheSolve: input is a list of access functions 
## produced by makeCacheMatrix. The output is the inverse of 
## x (input of function makeCacheMatrix). The output is calculated
## the first time the function runs. After that the 
## cached value is returned 

cacheSolve <- function(funs, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <<- funs$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- funs$get()
	inv <<- solve(data)
    funs$setinverse(inv)
    inv
}

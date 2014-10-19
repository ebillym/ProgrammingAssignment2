## Requirement
## Write the following functions:
##    
##    makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##    cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a matrix containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## Function makeCacheMatrix returns a special 2x2 "matrix"
##     value    inverse
## set set()    setinverse()       
## get get()    getinverse()

makeCacheMatrix <- function(x = matrix()) {
    ## Initiate inverse to NULL
    i <- NULL 
    ## set() sets the value of matrix
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    ## get() gets the value of matrix
    get <- function() x
    ## setinverse() set the value of inverse
    setinverse <- function(inverse) i <<- inverse
    ## getinverse() get the value of inverse
    getinverse <- function() i
    matrix(c(set=set, get=get, setinverse=setinverse, getinverse=getinverse),
           nrow = 2, ncol = 2,
           dimnames = list(c("set","get"),c("value", "inverse")))
}


## Takes the special matrix created by makeCacheMatrix()
## returns the inverse of the matrix from the cached value
## if it has already been calculated before, otherwise 
## solve the inverse and cache it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## if the inverse of x is not yet calculated
    ## calculate the inverse
    ## if inverse is already calculated, return the cached inverse
    i <- x[["get","inverse"]]()
    ## inverse already calculated, return cached value
    if(!is.null(i)){
        message("Returning cached inverse.")
        return(x[["get","inverse"]]())
    }
    ## if the inverse of x is not yet calculated
    ## calculate the inverse
    message("Calculating the inverse and cache it.")
    matrix <- x[["get","value"]]()
    inverse <- solve(matrix, ...)
    x[["set","inverse"]](inverse)
    inverse
}

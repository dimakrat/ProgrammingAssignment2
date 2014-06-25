## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix returns list of for functions
## function set() - write original matrix x and NULL to variable m to scope of the list 
## function get() - return original matrix x to the call scope
## function setsolve() - write inverted matrix to m to scope of the list
## function getsolve() - return inverted matrix m from scope of the list
## to the call scope.

MakeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## CacheSolve - invert matrix or read result from cach.
## If x$getsolve() return NULL, caculate inverce of matrix stored in list x 
## or read it from cache 

CacheSolve <- function(x, ...) {
        m <- x$getsolve()
    if(!is.null(m)) {
        message("cached data....")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}


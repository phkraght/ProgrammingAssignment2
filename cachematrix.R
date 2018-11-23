## This file contains functions required to performed 'cached' matrix inversion.

## makeCacheMatrix() creates a special "matrix" which is used by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  ## set inv; x is already set as a formal parameter
    
    ## define the four funtions
    set <- function(y) {  ## set is not used by cacheSolve(), but may be
                          ## called directly to reset the "matrix"
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(z) inv <<- z
    getInv <- function() inv

    ## return the list of functions so they may be used by cacheSolve()    
    list(set = set, get = get, 
         setInv = setInv, getInv = getInv)
}


## cacheSolve() either calculates the inverse, or retrieves the
## previously-calculated inverse.  The special "matrix" must first be
## created using makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)) {
        message('Getting chached inverse')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}

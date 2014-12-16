## FHSeymour, R Programming Assignment 2
##
## A two function combination that allows an invertible matrix to have its 
## inverse calculated once, cached, and read many times without re-calculation.
## The first function, makeCacheMatrix, is an object for storing the matrix,
## its inverse, and function definitions for caching and retrieving the inverse.  
## The second function cacheSolve, computes the inverse the first time it is 
## called and caches the results in the object.  On subsequent calls, it simply
## returns the already cached inverse result.


## makeCacheMatrix takes a matrix that is assumed to be
## square and non-singular (invertable) and returns a list of three methods
## get: to return the original matrix
## setInverse to cache the calculated inverted matrix values
## getInverse to retrieve the previously cached inverted matrix values

makeCacheMatrix <- function(m = matrix()) {
    
    # matrix inverse mInv initialized to NA
    # this is where the matrix inverse results will be stored
    mInv <- matrix(NA, nrow(m), ncol(m))    
    
    # get function/method definition to return matrix m
    get <- function() {m}
    
    # setInverse function/method definition to cache the inverse matrix values
    # in the makeCacheMatrix object.  The <<- operator is used to modify mInv 
    # in the parent of setInverse (makeCacheMatrix).
    # The actual matrix inversion calcs are performed outside of this method
    setInverse <- function(invertedValues) { mInv <<- invertedValues }
    
    # getInverse function/method definition returns cached inverse matrix
    # mInv from makeCacheMatrix is returned.
    getInverse <- function() { mInv }
    
    # the function/method/object makeCacheMatrix returns a list contained the 
    # methods for getting the matrix m, setting the cached inverse matrix mInv,
    # and getting the cached inverse matrix mInv
    list(get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## the function cacheSolve takes the object mCM created by makeCacheMatrix
## and on the first pass, computes the inverse and caches the result.
## on subsequent passes, it returns the cached result (no inverse recalc)

cacheSolve <- function(mCM, ...) {
    # first step is to retrieve the cached inverse matrix
    mInv <- mCM$getInverse()   
    
    if (is.na(mInv[1,1])) {
        # Assume that if first element of mInv is NA that 
        # the inverse matrix has not been calculated
        # retrieve the original matrix m and calculate inverse
        m <- mCM$get()       
        mInv <- solve(m, ...)
        # cache the results for future retrieval
        mCM$setInverse(mInv)        
        message("cacheSolve - first pass, calculating inverse")
    } else {
        # mInv has been previously calculated
        message("cacheSolve - retrieving inverse from cache")
    }
    
    # whether calculated or retrieved from cache, returns mCM matrix inverse
    mInv       
}


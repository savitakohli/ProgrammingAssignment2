## 1. Square matrix is passed as argument.
## 2. setmatrix function calculates Inverse of matrix
## 3. getmatrix function returns Inverse Matrix using function Solve.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    ##setmatrix function calculates inverse of matrix
    setmatrix<-function(solve) m<<- solve 
    ##getmatrix function returns inverse matrix
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
    
}

## 0. Function cacheSolve takes return value makeCacheMatrix (getmatrix) as argument, which is inverse of matrix passed as argument in makeCacheMatrix function.
## 1. Functions looks for the Cached value of the getmatrix.  If it is not NULL, returns the cached value.
## 2. If Cached value of the getmatrix is NULL, calculates  getmatrix and displays.
## 3. If this function is run again with same matrix, displays the cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    tst <- x$getmatrix()
    if (!is.null(tst)) {
        message("cached data avaialble")
        return(tst)
    }
    mat <- x$get()
    tst <- solve(mat, ...)
    x$setmatrix(tst)
    tst
}

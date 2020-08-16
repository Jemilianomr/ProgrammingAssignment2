## These functions allows you to create a "special" matrix
## (a list) of a real matrix and save in cache the
## inverse matrix of the first one.

## This function creates a list of functions to set and
## get the values of the matrix and the values of its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y){
        
        x<<-y
        inv<<-NULL
        
    }
    
    get <- function() x
    
    setinv <- function(inversa) inv <<- inversa 
    
    getinv <- function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
}


## This function returns the inverse of a matrix, if its
## values are already stored in cache it returns them, if not
## it makes the calculus 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if (!is.null(inv)){
        message("getting cache data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data,...)
    
    x$setinv(inv)
    
    inv
}

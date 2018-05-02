

#makeCacheMatrix function creates the matrix object first

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}
#list created which contains functions to set value of matrix, get value of matrix, set value for inverse of matrix and get the value for inverse of matrix

#cacheSolve function returns the inverse of matrix, checking if such already exists. If it does, i returns the inverse without having to calculate it.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached matrix...")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}

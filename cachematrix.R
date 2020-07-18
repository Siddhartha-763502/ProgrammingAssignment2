## The pair of functions below gives the inverse of the matrix and makes its calculation easier as these functions cache the inverse of the matrix.
## Use of these functions saves time as the repeated calculation is not needed.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {inv<- NULL
    set<- function(y){
      x<<- y
      inv<<- NULL
    }
    get<- function() {x}
    setInverse<- function(inverse) {inv<<- inverse}
    getInverse<- function() {inv}
    list(set= set, get= get, setInverse= setInverse, getInverse= getInverse)
}


## cacheSolve function computes the inverse of special "matrix" returned by above function. It will retrieve the inverse from the cache if the inverse has already been calculated

cacheSolve <- function(x, ...) {
       inv<- x$getInverse()
       if(!is.null(inv)) {
       message("getting cached data")
      return(inv)
    }
    mat<- x$get()
    inv<- solve(mat, ...)
    x$setInverse(inv)
    inv
}

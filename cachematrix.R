## makeCacheMatrix creates a special matrix object, i.e. a list of functions:
##      set: sets value of matrix
##      get: gets value of matrix
##      setinv: sets the value of the inverted matrix
##      getinv: gets the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invm) inv <<- invm
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the special matrix object x.
## First it checks, if the inverse has already been cached previously.
## If yes, it returns that value, if not it calculates the inverse
## with the solve() function and caches that result for later use

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Returning previously cached result.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## Creat matrix class, and set up its member functions as set,
## get, setInverse and getInverse.
## Also write a function which can read cached inverse of matrix, if the inverse does not exist, calculate it.


## This function creates a special matrix, which can store its inverse

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set <- function(y){
        x <<- y
        matrix_inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(mat_inverse) {
        matrix_inverse<<-mat_inverse
    }
    getInverse <- function() matrix_inverse
    list(set = set, get=get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function returns the inverse of matrix, firstly by looking up its cached inverse,
## if not found, calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat_inverse <- x$getInverse()
    if(!is.null(mat_inverse)){
        message("getting cached data")
        return(mat_inverse)
    }
    data <- x$get()
    mat_inverse <- solve(data)
    x$setInverse(mat_inverse)
    mat_inverse
}

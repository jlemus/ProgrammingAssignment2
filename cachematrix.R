## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function below creates a a special "matrix" object that can cache its 
# inverse. The special object is actually a list containing 4 functions
# that can:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# The function below receives as argument the special "matrix" object created
# by makeCacheMatrix and checks if the inverse of the matrix has already been
# calculated. If so, it uses the cached value in the special object. If not, it
# calculates the inverse of the matrix and sets the value of the inverse by
# calling the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

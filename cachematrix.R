## makeCacheMatrix stores in a vector all 4 functions that enable to
## store the value of a matrix, to retriev it, to store the value of
## the inverse of the matrix & to retrieve the value of the inversed
## matrix

makeCacheMatrix <- function(x = matrix()) {
# function set stores into cache the inverse matrix of y
        m <- NULL
        set <- function(y){
                y <<- x
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {
                m <<- inverse
        }
        getinverse <- function() m
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve check first if the inverse of matrix x has already been
## stored in cache. (if test is NULL, then it is the 1st time)
## If not, it computes the calculation and stores in x$setinverse()
## the result of the computation. So that if the inverse is requested
## again, it'll be stored in cache and accessible via the getinverse()
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            test <- x$getinverse()
        if(!is.null(test)){
                print('retrieving inverse matrix from cache')
                return (test)
        }
        # sinon
        test <- solve(x$get())
        x$setinverse(test)
        test
}

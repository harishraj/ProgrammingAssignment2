## The function here calculates the inverse of a matrix and saves it to the memory cache.
## If the user attempts to calculate the same matrix inverse, the previously saved value is returned from the cache
## rather than calculating it again.

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## create a matrix x
        
        m <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) m <<- inverse ## set the memory cache m equal to the inverse of the matrix x
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of matrix created with the function we have already created. 
## It first looks if the inverse has already been caclulated before, and takes the inverse from the memory cache
## and does not perform the calculation again. If it is not present, it calculates the inverse matrix and sets
## the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Returns the inverse matrix x
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting the cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Matrix inversion is a very costly computation. 
# The following two functions are designed to cache the inverse of a matrix instead of computing it repeatedly.

# The first function, makeCacheMatrix creates a special "matrix" object that caches input matrix and its inverse.
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
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#The second function, cacheSolve computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# It first checks whether the inverse has already been calculated. 
# If so, the inverse from the cache will be returned. 
# If not, the inverse will be calculated and setted in the cache via setinverse function.

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

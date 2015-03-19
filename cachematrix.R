#cachematrix 
##makeCacheMatrix: This function creates a special "matrix" object.
##cacheSolve calculates the inverse of the matrix
##If the matrix inverse is already calculated it will use the cache instead

makeCacheMatrix <- function(x = matrix()){
        i <- null
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function () i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve returns the inverse of the matrix
## If cached inverse is already solved and available it will be used and 
## not recalculated. If not it will be solved, cahced, and returned.

cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
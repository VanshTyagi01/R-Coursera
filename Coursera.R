makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the value of the inverse
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Retrieve the inverse from the cache
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Compute the inverse if it is not cached
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

# Example Usage

# Create a matrix
my_matrix <- matrix(c(1, 2, 3, 4), 2, 2)

# Create a special "matrix" object
cached_matrix <- makeCacheMatrix(my_matrix)

# Compute and cache the inverse
inverse_matrix <- cacheSolve(cached_matrix)

# Print the inverse
print(inverse_matrix)

# Get the cached inverse without recomputing
cached_inverse <- cacheSolve(cached_matrix)
print(cached_inverse)

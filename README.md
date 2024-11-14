# R-Coursera
</br>
makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
</br>
##Explanation
</br>
makeCacheMatrix:

Creates a list containing functions to set and get the matrix and its inverse.

set: Sets the matrix and clears the cached inverse.

get: Returns the matrix.

setinverse: Sets the cached inverse.

getinverse: Gets the cached inverse.

cacheSolve:

Checks if the inverse is already cached.

If cached, it retrieves and returns the inverse.

If not cached, it computes the inverse, caches it, and then returns it.

# makeCacheMatrix:function creates a special "matrix" object that can cache its inverse
# Below are the two functions that are used to create a special object that stroes matrix and cache's its inverse
# Set the value of the matrix
# Get the value of the matrix
# Set the value of the inverse
# Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) 
{
 # inv will store the cached inverse matrix
    inv <- NULL
 # Set the value of the matrix
  set <- function(y) 
{
	 x <<- y
    inv <<- NULL
    }
   #  Get the value of the matrix
   get <- function() x
 # Set the value of the inverse
   setinv <- function(inverse) inv <<- inverse
 #  Get the value of the inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}

## function computes the inverse of the matrix returned by "makeCacheMatrix" above
cacheSolve <- function(x, ...) 
{
    inv <- x$getinv()
# If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
 # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)
 # Cache the inverse
    x$setinv(inv)
    inv
}

# the values used for the calculations.Also check if the inverse has already been calculated then we see the message "getting cached data", now the "cacheSolve(cx)" will retrieve the inverse from the cache
 x <- matrix(1:4,2,2)
cx <- makeCacheMatrix(x)
cx$get()  
cacheSolve(cx) 
cacheSolve(cx)

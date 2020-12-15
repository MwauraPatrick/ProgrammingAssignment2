#Assume that the matrix supplied is always invertible.

#The following functions are used to create an object that stores a matrix 
#and caches its inverse. The first #function, makeCacheMatrix will creates a “matrix”, 
#which is a list containing a function to:

#1.set the value of the matrix

#2. get the value of the matrix

#3. set the value of the inverse

#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#This function will computes the inverse of the “matrix” returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  p <- x$getinverse()
  
  if (!is.null(p)){
    message("getting cached data")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}

##Testing the two functions

M <- matrix(c(1,2,3,4),2,2)

W <- makeCacheMatrix(M)

cacheSolve(W) #inverse returned after computation

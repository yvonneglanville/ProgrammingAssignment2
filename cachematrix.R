## This function creates a special matrix object, which 
## can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
    inversematrix <- NULL
    set <- function(y){
        x <<- y
        inversematrix <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inversematrix <<- solve(x)
    getinverse <- function() inversematrix
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)

}


## This function solves the inverse of the special matri
## returned by makeCacheMatrix.  If the inverse has already 
## been calculated and the matrix is unchanged the results of the inverse
## will be pulled from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if (!is.null(inversematrix)){
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data)
  x$setinverse(inversematrix)
  inversematrix
  
  
}

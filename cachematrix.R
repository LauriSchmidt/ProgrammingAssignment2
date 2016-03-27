## Create a matrix that can cache and another function to so we
## can access the cached version instead of re-calculating each time

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set <- function(y){
      x<<-y
      m<<-NULL
    }
    get <-function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Return the cached version of the inverse matrix if it exists; otherwise, 
## get the inverse of the matrix, cache it and then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m<-x$getinverse()
      if(!is.null(m)){
        message("Getting Cached data")
        return (m)
      }
      data <- x$get()
      m <-solve(data)
      x$setinverse(m)
      m
}



#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the matrix inverse
#get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  m_old <<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    m_old <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){
    
    m <<- inverse
    m_old <<- m
  }
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The following function calculates the mean of the special "vector" created with the above function. 
#However, it first checks to see if the mean has already been calculated. If so, it gets the mean 
#from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the 
#value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)|!identical(m,m_old)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

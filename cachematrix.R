#setinverse function sets the inverse of the function(from cache)
#getinverse function gets the inverse of the function
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(d) {
    x <<- d
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}
#cacheSolve checks first whether the matrix's inverse is calculated or not,if it
# is calculated,it returns inverse else it calculates its inverse and sets the 
# inverse i.e calls setinverse
# solve function directly gives the inverse of a matri
cacheSolve <- function(x, ...) {
m <- matrix()
m <- x$getinverse()
if(!is.null(m)) {
  message("getting cached data")
  return(m)
}
data <- x$get()
m <- solve(data)
x$setinverse(m)
m
}

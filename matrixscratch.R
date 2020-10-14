makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) m <<- inverse
  get.inv <- function() m
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}

cacheSolve <- function(x, ...) {
  m <- x$get.inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.inv(m)
  m
}

x <- matrix(1:4, 2, 2)
y <- makeCacheMatrix(x)
cacheSolve(y)
cacheSolve(y)
z <- y
cacheSolve(z)

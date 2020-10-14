## Put comments here that give an overall description of what your
## functions do

# There are two functions 1) makeCacheMatrix and 2) cacheSolve
# The first function initializes the variable m as NULL and 
# it also defines 4 different functions in its body
# the 2nd function will calculate the inverse of an n x n matrix
# However, before calculating the inverse it will check if 
# an inverse has already been calculated

# The idea behind these 2 functions is that if the calculation
# has already been performed then we can save computer resources

## Write a short comment describing this function

# As stated above, the makeCacheMatrix takes an n x n matrix as its only argument
# We assume that this matrix is invertible 
# The function goes on and it initializes a variable m as an empty variable; this
# will be used to save the actual inverse of the matrix x and returned to the console
# The makeCacheMatrix also defines four functions 

# 1) set function
# x is replaced with values of y (the argument of set) in the GLOBAL environment
# the variable m is "reset" to NULL however this time is set in the GLOBAL environment

# 2) get function
# this simply returns the original matrix x

# 3) set.inv function
# this function will save the inverse of matrix x to m in the Global environment 
# this will make sure that if we calculate the inverse then it is saved or "cache" 
# this way we don't have to calculate it again

# 4) get.inv function
# this simply gets the value of m regardless if it is NULL or m = inverse

makeCacheMatrix <- function(x = matrix()) {

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


## Write a short comment describing this function

# this is where the actual calculation of the inverse occurs
# the input of this function is the result (a list) of the function makeCacheMatrix
# the function will first set m = the result of the get.inv function 
# if m is not empty then this means we haved it "cached" and we can retrive it
# otherwise move on and calculate the inverse of x, save the result in GLOBAL
# this ensures that we have a "cache" version of the inverse and we don't 
# have to calculate it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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

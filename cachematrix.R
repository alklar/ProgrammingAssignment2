## Put comments here that give an overall description of what your
## functions do

## Example: matrix m 
## m <- = rbind(c(1, -1/4), c(-1/4, 1)) 
## create 'special matrix' my_matrix
## my_matrix <- makeCacheMatrix(m)
## use my_matrix as argument for cacheSolve
## cacheSolve(my_matrix)
##> my_matrix <- makeCacheMatrix(c)
##> cacheSolve(my_matrix)
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> cacheSolve(my_matrix)
##getting cached data
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667


## function to set and get a matrix 'x' and the inverse matrix 'inv'
## assumption: matrix supplied is always invertible
## this mean we can use solve() function for the inverse matrix 
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calls the makeCacheMatrix function
## if the result is cached 
## then cached result is returned
## otherwise calculate the inverse matrix, set and return it 

cacheSolve <- function(x, ...) {
  ## Return a matrix 'inv' that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

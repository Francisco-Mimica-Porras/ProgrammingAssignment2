
MakeCacheMatrix <- function (x= matrix ()){
  inv <- NULL
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  get <- function(){x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list (set=set, get=get, setinverse = setinverse, getinverse = getinverse)
  
}

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse (inv)
  inv
}

Pmatrix <-MakeCacheMatrix(matrix(1:14, nrow = 2, ncol = 2))
Pmatrix$get()
Pmatrix$getinverse()
cacheSolve(Pmatrix)
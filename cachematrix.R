makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(Set = set,Get = get, SetInverse = setInverse,GetInverse = getInverse)
}

cacheSolve <- function(x,...) {
  inver <- x$GetInverse()
  if (!is.null(inver)) {
    message("already get the cached data")
    return(inver)
  }
  matrix <- x$Get()
  inver <- solve(matrix,...)
  x$SetInverse(inver)
  inver
}
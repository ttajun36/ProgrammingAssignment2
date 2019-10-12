## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## when we put matrix x, it returns list 4 function that 1. change matrix 2. get matix
## 3. get Solve (solve = inverse matrix), 4. set Solve (solve = inverse matrix )  => it can save matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y = matrix()) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) m <<- solve
	getSolve <- function() m
	list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## Write a short comment describing this function
## input is the "special matrix" : list with 4 arguments that is produced by makeCacheMatrix function.
## it returns a matrix that is the inverse of 'x' by solve function, and save it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getSolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setSolve(m)
	m
}

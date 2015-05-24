## Owen Patrick - rprog-014
## makeCacheMatrix is an object that holds a matrix and the 
## cached results of its inverse matrix and determinant. The
## function cacheSolve checks, computes and writes the
## results of the determinant and inverse to the
## makeCacheMatrix object passed in.

## func: makeCacheMatrix
## Takes in a square matrix and constructs a class 
## wrapper to store the cached result of its Determinant and 
## Inverse, if the Inverse exists. Otherwise, it stores the 
## Determinant and the value "NA" for the Inverse.

makeCacheMatrix <- function(x = matrix()) {
	if(!is.matrix(x)) { message("Parameter is not a matrix."); return(NULL) }
	if(!is.atomic(x)) { message("Parameter is atomic."); return(NULL) }
	if((ncol(x) != nrow(x))) { message("Matrix is not square."); return(NULL) }

	result <- NULL
	dtmnt <- NULL
	
	set <- function(y) { ## y here is the incoming matrix
		x <<- y
		result <<- NULL
		dtmnt <<- NULL
	}
	
	get <- function() {
		x	
	}
	
	setInverse <- function(inverseData) {
		result <<- inverseData
	}
	
	getInverse <- function() {
		result
	}
	
	setDet <- function(detData) {
		dtmnt <<- detData
	}
	
	getDet <- function() {
		dtmnt
	}
	
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse,
		 setDet = setDet,
		 getDet = getDet)
}


## func: cacheSolve
## Takes in an instance of makeCacheMatrix and does the following:
## 1. Checks the determinant of the matrix in makeCacheMatrix class. 
## 2. If the determinant is not null, fetch the inverse result.
## 3. If it is null, compute and store it and then compute and store
## 	  the Inverse Matrix; NA if the determinant is 0.

cacheSolve <- function(x, ...) {
	result <- x$getDet()
	
	if(!is.null(result)) {
		message("getting cached data")
		return(x$getInverse())
	}
	
	data <- x$get()
	dtrmnt <- det(data)
	x$setDet(dtrmnt)	
	
	if(dtrmnt < 0.0000001 && dtrmnt > -0.0000001) {
		message("System is computationally singular OR exactly singular")
		x$setInverse(NA)
		return(NA)
	}
	
	result <- solve(data, ...)
	x$setInverse(result)
	
	result
}

makeCacheMatrix <- function(x = matrix()) {
		cached <- NULL 
		set <- function(y) {
			x <<- y
			cached <<- NULL
	}
	get <- function() {x} 
	setresult <- function(result) cached <<- result
	getresult <- function () cached  
	list(set = set, get = get, 
		setresult = setresult, 
		getresult = getresult)
}

cacheSolve <- function(x, ...) {
		if (!is.null(cached)) {
			message("returning cached result")
			return(cached)
		}
		matri <- x$get() 
		invmatri <- solve(matri,...) 
		x$setresult(invmatri) 
		invmatri 
}

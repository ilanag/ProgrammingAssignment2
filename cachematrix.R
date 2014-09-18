## Put comments here that give an overall description of what your
## functions do

## The functions below allow for a user to find the inverse matrix value and cache
## it for future ease of use. Caching reduces the computational power to complete
## calculations, as the program does not need to recaculate large values. 

## This script was built based on a sample script provided for 
## Programming Assignment 2 in the R Programming Course offerred 
## by Coursera and Johns Hopkins University in September 2014.

## Write a short comment describing this function
## The makeCacheMatrix:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        	set <- function(y) {
                	x <<- y
                	m <<- NULL
        	}
        	get <- function() x
        	setinverse <- function(inverse) m <<- inverse
        	getinverse <- function() m
        	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function sets the cached data to equal the inverse matrix of the supplied data, 
## the matrix created in the above function. The inverse is calculated if and only if
## the cached inverse is not yet created. If the cached inverse is already created, the
## function returns that value and skips the computation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

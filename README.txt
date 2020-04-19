## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
Testing My Functions
> source("ProgrammingAssignment2/cachematrix.R")
> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    2    1
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> cacheSolve(my_matrix)
getting cached data
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> my_matrix$getInverse()
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
Another Useful Example
A good example for getting a better understanding of differences amount formal parameters, local variables and free variables. And it is also helpful for learning the <<- operator.

open.account.R:

open.account <- function(total) {
        list(
                deposit = function(amount) {
                        if(amount <= 0)
                                stop("Deposits must be positive!\n")
                        total <<- total + amount
                        cat(amount, "deposited.  Your balance is", total, "\n\n")
                },
                withdraw = function(amount) {
                        if(amount > total)
                                stop("You don't have that much money!\n")
                        total <<- total - amount
                        cat(amount, "withdrawn.  Your balance is", total, "\n\n")
                },
                balance = function() {
                        cat("Your balance is", total, "\n\n")
                }
        )
}
Simple tests for open.account function:

P <- open.account(100)
> mm <- open.account(200)
> P$withdraw(30)
30 withdrawn.  Your balance is 70 

> P$balance()
Your balance is 70

> mm$balance()
Your balance is 200 

> P$deposit(50)
50 deposited.  Your balance is 120 

> P$balance()
Your balance is 120 

> P$withdraw(500)
Error in P$withdraw(500) : You don't have that much money!
> mm$balance()
Your balance is 200

> mm$withdraw(12)
12 withdrawn.  Your balance is 188 

> mm$balance()
Your balance is 188 

> P$balance()
Your balance is 120
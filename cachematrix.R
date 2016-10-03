## 2 functions for 'Programming Assignment 2' on Coursera 'R Programming" course
## Functions are aimed to create a matrix-like object and then compute a matrix inversion. Inverse is
## stored in cache and then retrieved without calculation if initial matrix was not changed 


## First function that creates a 'matrix-like' object. In reality a list of 4 functions. A matrix itself
## is stored in functions closure and can be also checked out by command
## get("x", environment(z$get)) ## z - 'matrix-like' object name

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL #set variable for solved matrix
    
    list(     #here is the list initiation
        get = function() x,    #'get' function, just returns the matrix
        set = function(m) {    
            x <<- m            #set a new matrix (and resets the solved matrix) 
            s <<- NULL         #by assigning value to 'upper' enviroment with '<<-'
        },
        getSolvedM = function() s, #returns solved matrix
        setSolvedM = function(solvedM) s <<- solvedM #assigns solved matrix
    )
}


## Second function computes the inversion of matrix or displays one directly from cache

cacheSolve <- function(y) {
    
    s <- y$getSolvedM() #get inversion (can be NULL)
    if(!is.null(s)) {
        message("getting cached data")
        return(s)      #if not NULL - returns the inversion
    }
    data <- y$get()    #otherwise stores initial matrix into local var
    s <- solve(data)   #stores computations into local var
    y$setSolvedM(s)    #load local var with computation result to cache
    s                  #displays result
}


###################################################
##        following part is for testing         ###
##             run line by line                 ###
###################################################

z <- makeCacheMatrix(matrix(rpois(49, 4), 7, 7))

z$get()
z$get()[1:5,1:5] #short version for big matrices

z$set(matrix(rpois(64, 6), 8, 8))
z$set(matrix(rpois(1000000, 10), 1000, 1000)) #to test with big matrix

z$getSolvedM()
z$getSolvedM()[1:5,1:5] #short version for big matrices

cacheSolve(z) 

##following 3 lines to be run together to observe proc.time difference
##between calculating vs. getting cached data
ptm <- proc.time()
cacheSolve(z)[1:5,1:5] #short version for big matrices
proc.time() - ptm

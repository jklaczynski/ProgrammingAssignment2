## Coursera "R" Programming class - week 3 homework assignment
## This script demonstrates the student's knowledge of 
## R scoping by implementing a simple caching algorithm.
##
## <RANT>
## However, The assignment is so poorly worded that a simple solution involving globals would work.
## BUT because PENG was lazy and didn't give a specification on usage of the function, I assume he
## wants the same type of embedding that the example showed.
## 
## Only by reading the example, can the student GUESS what the desired product should be.
##
## And that I have done.   
##
##  I therefore pledge to be merciful in the grading of others' work products.
##
## </RANT>
## 20 Feb 2016 J Klaczynski


## makeCacheMatrix() - create an matrix object that has cachable elements
## 
## This function creates what is better thought of as a class (as in Java or C++)
## with setters and getters
## Note that we define a set() to update the matrix, but the matrix could be updated outside
## of the set() method - if it is, then the cached inverse would not be valid and we 
## wouldn't know.
##
## We also provide setInverseCache() and getInverseCache() to set and return the cached inverse
## Finally, print() capability is provided for debugging.
## 
## the returned value 
## 

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse = NULL  #place to store the cached inverse matrix
    
    ## Setter function - store value of the matrix
    set <- function(newValue) {
        x <<- newValue
        cachedInverse <<- NULL   ## need to clear cache on update
    }
    
    ## getter function - more of a convenience for consistency
    get <- function() {
        x ## return origin matrix 
    }    
 
    ## cache getter function
    getInverseCache <- function () {
        cachedInverse
    }
    
    ## cache setter function
    setInverseCache <- function(inverse) {
        cachedInverse <<- inverse
    }
    
    ## print function - useful for debugging
    debugPrint <- function () {
        print (x)
        print (cachedInverse)
    }

    list(set = set, get=get, print=debugPrint,
         setInverseCache=setInverseCache,
         getInverseCache=getInverseCache)
}


## cacheSolve(m) - Return a matrix that is the inverse of 'x'
## if the inverse of matrix argument(m) HAS ALREADY been calculated and cached,
## this function returns previously calculated matrix, otherwise it calculates the inverse matrix
## and stores the result in the cache.

cacheSolve <- function(x, ...) {
    result <- x$getInverseCache()
    if (is.null(result)) {
        print ('Creating cache')
        result <- solve(x$get(), ...)
        x$setInverseCache(result)  
    }
    else {
        print('using Cache')
    }
  
  result

}


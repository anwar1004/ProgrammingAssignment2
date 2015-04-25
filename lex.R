crazy <- function() {
   x <- 3.14                   # variable x in the containing environment (global in this case) is updated to be 3.14
    print(x)                        # since no local variable ‘x’ exists within function ‘crazy’ R searches the containing environments
{ print(x);                     # this is to demonstrate the function, not a code block, is the smallest environment in R
  x <- 42; print(x)         # local variable ‘x’ is declared (created) and assigned the value 42; overrides the variable ‘x’ in
    }                                  # the containing environment
print(x)                       # since local variable ‘x’ now exists within the function there is no need to search the containing

}     

# https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/
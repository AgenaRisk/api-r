## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

Please find resubmission comments below. CRAN comments are in quotes, followed by our response.

> *Please always write package names, software names and API (application programming interface) names in single quotes in title and description.  
e.g: --> 'agena.ai'  
Please note that package names are case sensitive.*

Done, software titles are in single quotes in title and description.

> *Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)Missing Rd-tags:  
local_api_activate_license.Rd: \value    
local_api_clone.Rd: \value    
local_api_compile.Rd: \value    
login.Rd: \value*

Done, /value added to .Rd files to explain function results.

> *You write information messages to the console that cannot be easily suppressed.  
It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print()/cat() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions)
*

Done, all the console messages use message()/warning() so they can be supressed.

> *Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().*

The functions which need to create temporary files (to be deleted once the function operation is completed) are revised to write these files to tempdir() and not to the working directory or user’s filespace.

> *Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.  
e.g.:...  
oldwd <- getwd() # code line i  
on.exit(setwd(oldwd)) # code line i+1  
...  
setwd(...) # somewhere after  
...  
e.g.:  
If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.*

Done, where the code needs to alter the working directory momentarily (to access the installed local API directory), the on.exit() function is added immediately after.

> *Please do not modify the global environment (e.g. by using <<-) in your functions. This is not allowed by the CRAN policies.*

Only use of <<- occurs in reference class methods and is used to update reference class fields, which is required for the functionalities of the package. The functions do not modify the global environment otherwise, and it is never used outside the methods of a reference class.

## Resubmission

> *you missed to single quote one instance of 'agena.ai' in your description. Please add the quotes there as well.*

Thanks, fixed.

> *Also: Is there a link you can add as web reference for the API in the form \<https:.....\> to the description of the DESCRIPTION file with no space after 'https:' and angle brackets for auto-linking? If so please add it!*

Done, the first mention of agena.ai in the description now has the link to <https://agena.ai>.
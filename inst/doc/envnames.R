## ----echo=FALSE---------------------------------------------------------------
#library(knitr)
#opts_chunk$set(include=TRUE, warning=FALSE)

## ----echo=FALSE---------------------------------------------------------------
library(envnames)
rm(list=ls())

## ----Motivation---------------------------------------------------------------
myenv <- new.env()
cat("The name of the environment just defined is: ", environmentName(myenv), "(empty)\n")
cat("Simply referencing the environment just defined yields its memory address,
    which is not so helpful: "); print(myenv)
cat("Using the environment_name() function of the envnames package gives
    the environment name:", environment_name(myenv))

## ----DefineEnvironments-------------------------------------------------------
env1 <- new.env()
env_of_envs <- new.env()
with(env_of_envs, env21 <- new.env())

## ----GetLookupTable-----------------------------------------------------------
get_env_names()

## ----GetLookupTableRestricted-------------------------------------------------
get_env_names(envir=env_of_envs)

## ----GetEnvironmentNames, warning=FALSE---------------------------------------
cat("Name of environment 'env1':\n")
environment_name(env1)
cat("Name of environment 'env21':\n")
environment_name(env21)

## ----GetEnvironmentNamesSpecifyingLocation, warning=FALSE---------------------
cat("Name of environment 'env1' when we specify its location:\n")
environment_name(env1, envir=globalenv())
cat("Name of environment 'env21' when we specify its location:\n")
environment_name(env21, envir=env_of_envs)

## ----GetNameOfTestEnv---------------------------------------------------------
cat("Name of environment 'testenv':\n")
environment_name(testenv)

## ----GetNameOfNewEnvironmentThatPointsToAnExistingEnvironment-----------------
e_proxy <- env_of_envs$env21
environment_name(e_proxy)

## ----CallEnvironmentNameWithMatchNameTRUE1------------------------------------
environment_name(e_proxy, matchname=TRUE)

## ----CallEnvironmentNameWithMatchNameTRUE2------------------------------------
env_of_envs$e_proxy <- new.env()
environment_name(e_proxy, matchname=TRUE)

## ----CallEnvironmentNameOnNonExistingEnvironment, warning=FALSE---------------
environment_name(non_existing_env)

## ----ConvertMemoryAddressToEnvironmentName------------------------------------
env1_address = get_obj_address(testenv$env1)
environment_name(env1_address)

## -----------------------------------------------------------------------------
testenv$env1

## ----EnvironmentNameOfNonEnvironmentMemoryAddressIsNULL-----------------------
x = 2
environment_name(get_obj_address(x))

## ----GetExecutionEnvironmentName1---------------------------------------------
with(env_of_envs$env21, {
  f <- function() {
    cat("1) We are inside function:", environment_name(), "\n")
    cat("2) The calling environment is:", environment_name(parent.frame()), "\n")
  }
  g <- function() {
    f()
  }
})
cat("Having defined both f() and g() in environment env_of_envs$env21,
    and having function g() call f()...\n")
cat("...when we call env_of_envs$env21$f() from the global environment,
    we get the output that follows:\n")
env_of_envs$env21$f()
cat("\n...and when we call f() from inside function g(),
    we get the output that follows:\n")
env_of_envs$env21$g()

## ----GetExecutionEnvironmentName2---------------------------------------------
with(env_of_envs$env21, {
  f <- function() {
    cat("1) We are inside function", environment_name(), "\n")
    cat("2) The calling environment is:", environment_name(parent.frame()), "\n")
  }
  h <- function() {
    env_of_envs$env21$f()
  }
  }
)
env_of_envs$env21$h()

## ----DefineObjectsInEnvironments----------------------------------------------
x <- 5
env1$x <- 3
with(env_of_envs, env21$y <- 5)
with(env1, {
  vars_as_string <- c("x", "y", "z")
})

## ----LookForObjects1----------------------------------------------------------
environments_where_obj_x_is_found = obj_find(x)
cat("Object 'x' found
in the following environments:"); print(environments_where_obj_x_is_found)
environments_where_obj_y_is_found = obj_find(y)
cat("Object 'y' found
in the following environments:"); print(environments_where_obj_y_is_found)

## ----LookForObjects2----------------------------------------------------------
environments_where_obj_is_found = obj_find(vars_as_string)
cat("Object 'vars_as_string' found
in the following environments:"); print(environments_where_obj_is_found)

## ----LookForObjectsWhoseNamesAreGivenInArray----------------------------------
environments_where_obj_1_is_found = obj_find(env1$vars_as_string[1])
  ## Here we are looking for the object 'x'
cat(paste("Object '", env1$vars_as_string[1], "' found
in the following environments:")); print(environments_where_obj_1_is_found)
environments_where_obj_2_is_found = obj_find(env1$vars_as_string[2])
  ## Here we are looking for the object 'y'
cat(paste("Object '", env1$vars_as_string[2], "' found
in the following environments:")); print(environments_where_obj_2_is_found)
environments_where_obj_3_is_found = obj_find(env1$vars_as_string[3])
  ## Here we are looking for the object 'z' which does not exist
cat(paste("Object '", env1$vars_as_string[3], "' found
in the following environments:")); print(environments_where_obj_3_is_found)

## ----LookForObjectsUsingSAPPLY------------------------------------------------
environments_where_objs_are_found = with(env1, sapply(vars_as_string, obj_find) )
cat("The objects defined in the 'env1$vars_as_string' array are found
    in the following environments:\n");
print(environments_where_objs_are_found)

## ----LookForObjectsUsingSAPPLYNoGlobalSearch----------------------------------
environments_where_objs_are_found = with(env1,
                      sapply(vars_as_string, obj_find, globalsearch=FALSE, envir=env1) )
cat("The objects defined in the 'env1$vars_as_string' array are found
    in the following environments (no globalsearch):\n");
print(environments_where_objs_are_found)

## ----LookForObjectAsASymbol---------------------------------------------------
environments_where_obj_x_is_found = obj_find(as.name("x"))
cat("Object 'x' found in the following environments:\n")
print(environments_where_obj_x_is_found)

## ----LookForObjectsDefinedInPackages------------------------------------------
environments_where_obj_is_found = obj_find(aov)
cat("Object 'aov' found in the following environments:\n")
print(environments_where_obj_is_found)

## ----DefineTwoEnvironments----------------------------------------------------
env11 <- new.env()
env12 <- new.env()

## ----DefineFunctionH----------------------------------------------------------
with(globalenv(), 
h <- function(x, silent=TRUE) {
  fun_calling_chain = get_fun_calling_chain(silent=silent)

  # Do a different operation on input parameter x depending on the calling function
  fun_calling = get_fun_calling(showParameters=FALSE)
  if (fun_calling == "env11$f") { x = x + 1 }
  else if (fun_calling == "env12$f") { x = x + 2 }

  return(x)
}
)

## ----DefineTwoFunctionsFInSeparateEnvironments--------------------------------
with(env11,
  f <- function(x, silent=TRUE) {
    fun_calling_chain = get_fun_calling_chain()
    return(h(x, silent=silent))
  }
)

with(env12,
  f <- function(x, silent=TRUE) {
    fun_calling_chain = get_fun_calling_chain()
    return(h(x, silent=silent))
  }
)

## ----RunFunctionF1, echo=FALSE------------------------------------------------
silent = FALSE
x = 0
cat("\nWhen h(x) is called by env11$f(x=", x, ") the output is: ", env11$f(x, silent=silent), "\n", sep="")

## ----RunFunctionF2, echo=FALSE------------------------------------------------
silent = FALSE
x = 0
cat("\nWhen h(x) is called by env12$f(x=", x, ") the output is: ", env12$f(x, silent=silent), "\n", sep="")

## ----GetFunEnv----------------------------------------------------------------
h <- function(x) {
  # Get the value of parameter 'x' in the execution environment of function 'env1$g'
  # The returned value is a list because there may exist different instances of the
  # same function.
  xval_h = x
  xval_g = evalq(x, get_fun_env("env1$g")[[1]])
  cat("The value of variable 'x' in function", get_fun_name(), "is", xval_h, "\n")
  cat("The value of variable 'x' inside function env1$g is", xval_g, "\n") 
}
env1 <- new.env()
with(env1, 
  g <- function() {
    x = 2
    return( h(3) )
  }
)
env1$g() 

## ----GetFunEnvOutside---------------------------------------------------------
cat("The execution environment of a function that is not in the calling chain is:\n")
print(get_fun_env("env1$g"))


## ----GetFunEnvCombinedExample-------------------------------------------------
h <- function(x) {
  parent_function_name = get_fun_calling(n=1)
  cat("Using get_fun_calling() and environment_name() functions:
      The parent frame of function", get_fun_name(), "is", get_fun_calling(n=2), "\n")
  # Get the value of parameter 'x' in the execution environment of function 'env1$g'
  # The returned value is a list because there may exist different instances of the
  # same function.
  xval_h = x
  xval_g = evalq(x, get_fun_env(parent_function_name)[[1]])
  cat("Using get_fun_name():
      The value of variable 'x' in function", get_fun_name(), "is", xval_h, "\n")
  cat("Using get_fun_env() and evalq() functions:
      The value of variable 'x' inside function", parent_function_name, "is", xval_g,"\n") 
}
env1 <- new.env()
with(env1, 
  g <- function() {
    x = 2
    return( h(3) )
  }
)
env1$g() 

## ----GetObjNameExampleDefinitions---------------------------------------------
getObjNameAndCompareWithSubstitute <- function(y, eval=FALSE) {
  parent_generation = 2
  get_obj_name_result = get_obj_name(y, n=parent_generation, eval=eval)
  deparse_result = deparse(y)
  substitute_result = substitute(y, parent.frame(n=parent_generation))
  deparse_substitute_result = deparse(substitute(y, parent.frame(n=parent_generation)))
  eval_result = evalq(y, envir=parent.frame(n=parent_generation))
  if (!eval) {
    cat("Result of get_obj_name(y, n=", parent_generation, "): ", get_obj_name_result,
        "\n\tConceptually this is the name of the object at parent generation ",
        parent_generation,
        "\n\tLEADING to *parameter* 'y'.\n", sep="")
    cat("Result of deparse(substitute(y, parent.frame(n=", parent_generation, "))): ",
        deparse_substitute_result,
        "\n\tConceptually this is the substitution of *variable* 'y'
        at parent generation ", parent_generation,
        "\n\tconverted to a string.\n", sep="")
  } else {
    cat("Result of get_obj_name(y, n=", parent_generation, ", eval=", eval, "): ",
        get_obj_name_result,
        "\n\tConceptually this is the object LEADING to *parameter* 'y' evaluated
        at parent generation ", parent_generation, ".\n", sep="")
    cat("Result of deparse(y): ", deparse_result,
        "\n\tConceptually this is the value of *parameter* 'y' converted to a character
        string.\n", sep="")
    cat("Result of substitute(y, parent.frame(n=", parent_generation, ")): ",
        substitute_result,
        "\n\tConceptually this is the substitution of *variable* 'y' at parent generation ",
        parent_generation,
        ".\n", sep="")
    cat("Result of evalq(y, envir=parent.frame(n=", parent_generation, ")): ",
        eval_result,
        "\n\tConceptually this is the evaluation of *variable* 'y' at parent generation ",
        parent_generation,
        ".\n", sep="")
  }
}

callGetObjNameAndCompareWithSubstitute <- function(x, eval=FALSE) {
  getObjNameAndCompareWithSubstitute(x, eval=eval)
}

## ----GetObjNameExampleCall1---------------------------------------------------
y <- -9   # Global variable with the same name as the parameter of testing function
z <- 3
callGetObjNameAndCompareWithSubstitute(z)

## ----GetObjNameExampleCall2---------------------------------------------------
y <- -9   # Global variable with the same name as the parameter of testing function
z <- 3
callGetObjNameAndCompareWithSubstitute(z, eval=TRUE)

## ----RetrieveParameterPath----------------------------------------------------
f1 <- function(x) {
  cat("f1(x) is calling f2(y=x)...\n")
  f2(x)
}
f2 <- function(y) {
  cat("f2(y) is calling f3(z=y)...\n")
  f3(y)
}
f3 <- function(z) {
  cat("f3(z) is retrieving the parameter path from three parent environments
      leading to function parameter z...\n\n")
  cat("Output from get_obj_name(z, n=3, silent=FALSE):\n")
  variable_leading_to_z_3levels_back = get_obj_name(z, n=3, silent=FALSE)
}
w = 1.3
f1(w)

## ----GetObjValueExampleDefinitions--------------------------------------------
getObjValueAndCompareWithEval <- function(y) {
  parent_generation = 2
  get_obj_value_result = get_obj_value(y, n=parent_generation)
  eval_result = evalq(y, envir=parent.frame(n=parent_generation))
  cat("Result of get_obj_value(y, n=", parent_generation, "): ", get_obj_value_result,
      "\n\tConceptually this is the object LEADING to *parameter* 'y' 
      \tevaluated at parent generation ",
      parent_generation, ".\n", sep="")
  cat("Result of evalq(y, envir=parent.frame(n=", parent_generation, ")): ", eval_result,
        "\n\tConceptually this is the evaluation of *variable* 'y' at parent generation ",
        parent_generation, ".\n", sep="")
}

callGetObjValueAndCompareWithEval <- function(x) { getObjValueAndCompareWithEval(x) }

## ----GetObjValueExampleCall---------------------------------------------------
y <- -9   # Global variable with the same name as the parameter of testing function
z <- 3
callGetObjValueAndCompareWithEval(z)

## ----GetObjectAddress1--------------------------------------------------------
obj_address1 = get_obj_address(x)
cat("Output of 'get_obj_address(x)':\n"); print(obj_address1)
obj_address2 = with(env1, get_obj_address(x))
cat("Output of 'with(env1, get_obj_address(x))':\n"); print(obj_address2)

## ----GetObjectAddress2--------------------------------------------------------
get_obj_address(env1$x)
get_obj_address(x, envir=env1)
with(env1, get_obj_address(x, envir=env1))

## ----GetNonExistentObjectAddress----------------------------------------------
vars = c("x", "y", "nonexistent")
get_obj_address(vars[1], envir=env1)
sapply(vars, get_obj_address)

## ----CheckMemoryAddressIsCorrect----------------------------------------------
address(env1$x)
address(e_proxy$y)

## ----MemoryAddressOfNULL------------------------------------------------------
address(env1$nonexistent)
address(NULL)

## ----MemoryAddressOfNonExistentIsNULL-----------------------------------------
get_obj_address(env1$nonexistent)

## ----SystemInfo, echo=FALSE---------------------------------------------------
data.frame(SystemInfo=Sys.info()[c("sysname", "release", "version", "machine")])
version


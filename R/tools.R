delayedAssign("LOG", logger_factory("Tools"))

#' Execute a Function from a Dynamically Loaded Box Module
#'
#' This function dynamically loads a specified module using `box`, retrieves a
#' specific function from it, and executes that function with the provided
#' arguments.
#'
#' @param mod A character string specifying the module path or name.
#' @param func A character string specifying the function name to retrieve from the module.
#' @param args A list of arguments to pass to the function.
#'
#' @return The result of the executed function, or `NULL` if an error occurs.
#' @export
execute_box_mod_func <- function(mod, func, args) {
  stopifnot(is.character(mod), is.character(func), is.list(args))

  tryCatch(
    {
      box_expr <- sprintf("box::use(%s[%s])", mod, func)
      eval(parse(text = box_expr), envir = environment())
    },
    error = function(e) {
      LOG$error(glue::glue("Failed to load module '{mod}': {e$message}"))
    }
  )

  # Retrieve the function object bound into the current environment by box::use()
  func_obj <- get(func, envir = environment())

  # Validate that the retrieved object is indeed a function
  if (!is.function(func_obj)) {
    LOG$error(glue::glue("'{func}' from module '{mod}' is not a function."))
  }

  # Execute the function with provided arguments
  tryCatch(
    do.call(func_obj, args),
    error = function(e) {
      LOG$error(glue::glue("Error executing '{func}': {e$message}"))
    }
  )
}

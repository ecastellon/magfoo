# -*- coding:utf-8 -*-

## private functions

#' length
#' @description vector has length greater than zero?
#' @param x vector
#' @return logical
#' @author eddy castellón
filled <- function(x) {
    length(x) > 0
}

#' length
#' @description vector's length equal to zero?
#' @param x vector
#' @return logical
#' @author eddy castellón
empty <- function(x) {
    length(x) == 0
}

#' character type
#' @description vector is of character type and has elements?
#' @param x vector
#' @return logical
filled_char <- function(x) {
    is.character(x) && length(x)
}

#' numeric mode
#' @description vector is of numeric mode and has elements?
#' @param x vector
#' @return logical
filled_num <- function(x) {
    is.numeric(x) && length(x)
}

#' integer type
#' @description vector is of integer type and has elements?
#' @param x vector
#' @return logical
filled_int <- function(x) {
    is.integer(x) && length(x)
}

#' valida-name
#' @description Valida nombres de variables
#' @param x vector
#' @return logical
#' @keywords internal
is_name <- function(x) {
    length(x) && identical(x, make.names(x))
}

#' nombre-scalar
#' @description Valida nombre escalar
#' @param x vector
#' @return logical
#' @keywords internal
is_scalar_name <- function(x) {
    length(x) == 1L && identical(x, make.names(x))
}

#' path maybe
#' @description test string begins with one o more alphanumeric
#'     characters followed by file path separators, or it begins with
#'     "./" or ".\\" followed by one or more alpanumeric characters;
#'     in both cases ending in alphanumeric character.
#' @param x character
#' @return TRUE if x has at least one slash followed by and ended by
#'     an alphanumeric character
#' @keywords internal
#' @examples
#' \dontrun{
#' is_path(".aa/bb")
#' is_path("aa/bb")
#' is_path("aa/bb.")
#' }
is_path <- function(x) {
    filled_char(x) && grepl("^((\\w+[/\\])|(\\.?[/\\]\\w+)).+\\w$", x)
}

#' file name
#' @description check file's name is valid using the function
#' \code{file.create} of base R. If any directory in the path chain
#' doesn't exists, the file's name is invalid.
#' @param x character; the file's name
#' @return logical
#' @author eddy castellón
ok_fname <- function(x = character()) {
    ok <- file.exists(x)
    if (!ok) {
        ok <- file.create(x)
        if (ok) {
            unlink(x)
        }
    }
    return(ok)
}

#' check call
#' @description check arguments in a call
#' @param x arguments
#' @return logical
chk_vector_call <- function(x) TRUE

#' dot argument
#' @description arguments in ... returned as a character or integer
#'     vector
#' @param ... arguments
#' @return \code{NULL}, character or integer vector
#' @author eddy castellón
#' @examples
#' \dontrun{
#' dots_arg(a, b)
#' dots_arg("a", "b")
#' dots_arg(c("a", "b"))
#' dots_arg(1:3)
#' }
dots_arg <- function(...) {
    xp <- eval(substitute(alist(...)))
    nn <- length(xp)

    if (nn > 1L) {
        vapply(xp, as.character, "a")
    } else {
        if (nn == 1L) {
            if (inherits(xp[[1]], "call")) {
                ## !!!
                ## check is c(..) or seq(., .)
                ## log(.) or similar error
                if (chk_vector_call(xp[[1]])) {
                    ex <- eval(xp[[1]])
                    if (is.numeric(ex)) {
                        return(NULL)
                    } else {
                        return(ex)
                    }
                } else {
                    NULL
                }
            } else {
                as.character(xp[[1]])
            }
        } else {
            message("\n!!! with out arguments")
            NULL
        }
    }
}

#' save
#' @description save without errors?
#' @param ... arguments passed to function save
#' @return logical; FALSE if save with errors
#' @author eddy castellón
save_ok <- function(...) {
    !inherits(try(save(...)), "try-error")
}

#' load file
#' @description load a file catching errors
#' @param x character; file's name
#' @param env object environment where objects are loaded;
#' \code{parent.frame} by default
#' @return character; objects' names or NULL
#' @examples
#' \dontrun{
#' try_load("xx.rda")
#' try_load("xx.rda", env = new.env())
#' }
#' @author eddy castellón
try_load <- function(x, env = parent.frame()) {
    if (missing(x) || !filled_char(x)) {
        message("... falta nombre de archivo !!!")
        return(NULL)
    }

    if (!is.environment(env)) {
        message("... argumento NO es un environment !!!")
        return(NULL)
    }

    tryCatch(load(x, envir = env),
        error = function(e) {
            if (file.exists(x)) {
                message("... ERROR durante lectura !!!")
            } else {
                message("... archivo no existe !!!")
            }
            return(NULL)
        },
        warning = function(e) {
            if (file.exists(x)) {
                message("... ERROR durante lectura !!!")
            } else {
                message("... archivo no existe !!!")
            }
            return(NULL)
        }
    )
}

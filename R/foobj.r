# -*- coding: utf-8 -*-

#' Metadatos - Asignar
#' @description Le asigna a un objeto, una breve descripción o
#'     metadatos.
#' @details Al objeto se le agrega el atributo "meta" para registrar
#'     una breve descripción (metadatos), o el nombre de un archivo en
#'     el que se hace una descripción más extensa. Ese nombre debe
#'     contener al menos una pleca ("/") para que la función no
#'     interprete el nombre como metadatos. El contenido del archivo
#'     debe apegarse al formato "DCF", explicado en la ayuda de la
#'     función \code{read.dcf}.
#' 
#'     Es una alternativa de la función \code{comment}.
#' @param x objeto
#' @param value character: metadatos
#' @seealso \code{comment}, \code{read.dcf}
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' meta(df) <- "./metadata.txt"
#' meta(df) <- "other_path/metadata.txt"
#'
#' rm(df)
#' @author Eddy Castellón
`meta<-` <- function(x, value) {
    attr(x, "meta") <- value
    invisible(x)
}

#' Metadatos - Leer
#' @description Lee el contenido del atributo "meta" o del archivo
#'     cuyo nombre está registrado en el atributo.
#' @param x nombre del objeto
#' @param read_me logical. Si TRUE y el atributo "meta" es nombre de
#'     archivo, devuelve el contenido del archivo. FALSE por omisión.
#' @return string o \code{NA} si el objeto no tiene atributo "meta"
#' @seealso \code{comment}, \code{read.dcf}
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' meta(df)
#'
#' rm(df)
#' @author Eddy Castellón
meta <- function(x, read_me = FALSE) {
    cc <- attr(x, "meta")
    if (is.null(cc)) {
        cc <- NA_character_
    } else {
        if (read_me) {
            if (is_path(cc) && file.access(cc, mode = 4L) == 0) {
                cc <- try(read.cdf(cc))
            } else {
                message("\n... acceso al archivo, denegado !!!")
            }
        }
    }
    cc
}

#' Objetos almacenados
#' @description Nombres de los objetos almacenados en un archivo.
#' @param file character. Nombre del archivo
#' @param meta logical. Si TRUE (default) devuelve valor del atributo
#'     "meta" de cada objeto.
#' @param class character. Hace el reporte sólo para objetos de clase
#'     "class". Por omisión, objetos de cualquier clase (class = ".").
#' @return character si meta = FALSE, o data.frame con una columna para
#'     el nombre y otra para los metadatos; \code{NULL} si ocurre un
#'     error.
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#'
#' list_off(fi, meta = FALSE)
#' list_off(fi, class = "data.frame")
#'
#' unlink(fi)
#' rm(df, d2, vi, fi)
#' @author Eddy Castellón
list_off <- function(file, meta = TRUE, class = ".") {
    ANY <- "."
    anyclas <- class == ANY

    ne <- new.env(parent = emptyenv())
    cc <- try_load(file, ne)
    if (is.null(cc)) {
        return(cc)
    }
    ## !!!
    ## si nn = 0 no hay objetos en file?
    ## y si file no es un archivo R

    cc <- cc[order(cc)]
    nn <- length(cc)
    clases <- character(nn)
    metas <- character(nn)
    keep <- logical(nn)

    for (jj in seq_along(cc)) {
        zz <- eval(as.name(cc[jj]), envir = ne)
        keep[jj] <- anyclas || inherits(zz, class)
        clases[jj] <- paste(class(zz), collapse = ",")
        metas[jj] <- ifelse(is.null(me <- attr(zz, "meta")),
            NA_character_, me
        )
    }

    if (any(keep)) {
        if (meta) {
            xx <- data.frame(
                data = cc[keep],
                meta = metas[keep],
                stringsAsFactors = FALSE
            )
            if (anyclas) {
                return(cbind(xx,
                    class = clases[keep],
                    stringsAsFactors = FALSE
                ))
            } else {
                return(xx)
            }
        } else {
            if (anyclas) {
                return(data.frame(
                    data = cc[keep],
                    class = clases[keep],
                    stringsAsFactors = FALSE
                ))
            } else {
                return(cc[keep])
            }
        }
    } else {
        message("\nno hay objetos de la clase ", class)
        return(NULL)
    }
}

#' data.frames almacenados
#' @description Data frames almacenados en un archivo.
#' @details Alias de la función \code{list_off} con argumento
#'     "data.frame" en el parámetro "class".
#' @param file character: nombre del archivo
#' @param meta logical: devolver metadatos? (TRUE por defecto)
#' @return character or data.frame
#' @seealso \code{list_off}, \code{meta}
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#'
#' list_dff(fi)
#' unlink(fi)
#' rm(df, d2, vi, fi)
#' @author Eddy Castellón
list_dff <- function(file, meta = TRUE) {
    list_off(file, meta, class = "data.frame")
}

#' Copiar objetos
#' @description Copia los objetos almacenados en un archivo, a un
#'     \code{environment} específico.
#' @param ... nombres de los objetos. Si ausente, se copian todos los
#'     objetos.
#' @param file character: nombre del archivo
#' @param class character: clase de los objetos a copiar. Por omisión,
#'     cualquier clase (\code{class="."}).
#' @param env environment: \code{environment} donde serán copiados los
#'     objetos. Por omisión, el ambiente desde donde se llama la
#'     función (\code{parent.frame}
#' @return character: nombre de los objetos copiados o \code{NULL}
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#'
#' rd <- read_off(file = fi, class = "data.frame")
#' exists(rd[1])
#'
#' rd <- read_off(c("d2", "vi"), file = fi)
#' class(rd[2])
#'
#' nwe <- new.env()
#' rd <- read_off(d2, file = fi, env = nwe)
#' exists("d2")
#' exists("d2", envir = nwe)
#'
#' unlink(fi)
#' rm(df, d2, vi, fi, nwe, rd)
#' @author eddy castellón
read_off <- function(..., file = character(), class = ".",
                     env = parent.frame()) {
    ANY <- "."
    if (!is.environment(env)) {
        message("!!! environment no existe")
        return(NULL)
    }

    ne <- new.env(parent = emptyenv())
    oo <- try_load(file, ne) # load to env could overwrite

    ## objects in file
    if (!is.null(oo)) {
        if (!missing(...)) {
            oo <- intersect(oo, dots_arg(...))
            if (empty(oo)) { # there is not objects in ...
                oo <- NULL
            }
        }
    }

    ## objects of class
    if (!is.null(oo)) {
        if (class == ANY) {
            class <- "any"
        }

        ok <- logical(length(oo)) # track class
        for (kk in seq_along(oo)) {
            ob <- get(oo[kk], envir = ne, inherits = FALSE)
            if (ok[kk] <- class == "any" || inherits(ob, class)) {
                assign(oo[kk], ob, pos = env)
            }
        }

        oo <- oo[ok]
        if (empty(oo)) { # not one with class
            oo <- NULL
        }
    }
    return(oo)
}

#' Leer data.frame
#' @description Lee uno o más data.frame almacenados en archivo.
#' @details Lee los data.frame y los copia en el \code{environment}
#'     indicado en el parámetro \code{env}. Llama la función
#'     \code{read_off} con argumento "data.frame" en el parámetro
#'     \code{class}.
#' @param ... nombres de los data frames. Si ausente, se leen todos
#'     los que están en el archivo
#' @param file character: nombre del archivo
#' @param env environment. Por omisión, el \code{parent.frame} (desde
#'     donde se llama la función
#' @return character con los nombres de los data.frame o \code{NULL}
#' @seealso \code{read_off}
#' @export
#' @author eddy castellón
read_dff <- function(..., file, env = parent.frame()) {
    if (missing(...)) {
        oo <- read_off(file = file, env = env, class = "data.frame")
    } else {
        oo <- read_off(...,
            file = file, env = env,
            class = "data.frame"
        )
    }

    return(oo)
}

#' Lee y asigna objetos
#' @description Lee uno o más objetos almacenados en archivo y los
#'     asigna (bind) a una variable. La variable será un objeto de
#'     clase lista si son leídos dos o más objetos.
#' @param ... nombres de objetos. Si ausente, lee todos los
#'     almacenados
#' @param file character: nombre del archivo
#' @param class character: clase de los objetos. Por omisión, de
#'     cualquier clase (\code{class = "."})
#' @seealso \code{read_off}
#' @return objeto, lista de objectos o \code{NULL}
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#'
#' rd <- get_off(file = fi)
#' sapply(rd, class)
#'
#' rd <- get_off(file = fi, class = "data.frame")
#' length(rd)
#' names(rd[[1]])
#'
#' rd <- get_off(d2, file = fi)
#' names(rd)
#'
#' rd <- get_off(vi, file = fi, class = "integer")
#' length(rd)
#'
#' unlink(fi)
#' rm(df, d2, vi, rd, fi)
#' @export
#' @author Eddy Castellón
get_off <- function(..., file = character(), class = ".") {
    ne <- new.env(parent = emptyenv())
    if (missing(...)) {
        oo <- read_off(file = file, env = ne, class = class)
    } else {
        oo <- read_off(..., file = file, env = ne, class = class)
    }

    if (is.null(oo)) {
        return(oo)
    } else {
        ob <- as.list(ne)
        if (length(ob) == 1L) ob <- ob[[1]]
        invisible(ob)
    }
}

#' Lee - asigna data.frames
#' @description Lee uno o más data.frame y los asigna a una variable.
#' @details Es alias de la función \code{get_off} con argumento
#'     "data.frame" en el parámetro "class", pero con el parámetro
#'     "file" al inicio, para que no sea necesario escribirlo.
#' @param file character: nombre del archivo
#' @param ... nombres de los data.frame o vector tipo character con
#'     los nombres. Si ausente, se leen todos los data.frame en el
#'     archivo.
#' @return data.frame, lista de data frames o \code{NULL}
#' @seealso \code{get_off}
#' @examples
#' \dontrun{
#'   list_dff("path/file")
#'   # "df1", "df2"
#'   x <- get_dffs("path/file", df2)
#'   y <- get_dffs("path/file", df2, df1)
#' }
#' @export
#' @author eddy castellón
get_dffs <- function(file, ...) {
    if (missing(...)) {
        oo <- get_off(file = file, class = "data.frame")
    } else {
        oo <- get_off(..., file = file, class = "data.frame")
    }

    if (is.null(oo)) {
        return(oo)
    } else {
        invisible(oo)
    }
}

#' Lee - asigna un data.frame
#' @description Lee un data frame y lo asocia con una variable
#' @param x nombre del data frame o character con el nombre
#' @param file character: nombre del archivo
#' @return a data.frame o NULL
#' @seealso \code{get_dffs} \code{get_off}
#' @export
#' @author eddy castellón
get_dff <- function(x, file) {
    df <- as.character(substitute(x))
    oo <- do.call("get_off", list(df, file = file, class = "data.frame"))
    if (is.null(oo)) {
        return(oo)
    } else {
        invisible(oo)
    }
}

#' Remover - objeto
#' @description Remueve objetos de un archivo y guarda los restantes
#' en el mismo o en otro archivo
#' @param x nombre del objeto
#' @param file character: nombre del archivo que almacena objetos.
#' @param file2 character: nombre del archivo donde se almacenarán los
#' objetos que no son removidos. Si ausente, los objetos seguirán
#' almacenados en el mismo archivo.
#' @return character: nombres de los objetos no removidos o
#' \code{NULL} si algún error durante el proceso
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, d2, vi, file = fi)
#'
#' rd <- rm_off(vi, file = fi)
#' rd
#'
#' unlink(fi)
#' rm(rd, fi, df, d2, vi)
#' @export
#' @author Eddy Castellón
rm_off <- function(x, file = character(), file2) {
    ne <- new.env(parent = emptyenv())
    oo <- try_load(file, ne)

    if (!is.null(oo)) {
        if (missing(file2)) file2 <- file
        ob <- as.character(substitute(x))

        if (is.element(ob, oo)) {
            warning("\n!!!! ... ", ob, " removido de ", file)
            rm(list = ob, envir = ne)
            oo <- ls(ne, all.names = TRUE)
            save(list = oo, file = file2, envir = ne)
        } else {
            message("... objeto ", x, " no encontrado !!!")
        }
    }
    oo
}

#' Agregar objetos
#' @description Agrega objetos a un archivo sin eliminar los
#'     previamente almacenados en él; pero objetos con el mismo nombre
#'     son remplazados.
#' @param ... nombres de los objetos o un vector tipo character con
#'     los nombres de los objetos que serán agregados
#' @param file character: nombre del archivo. Si no existe, es creado
#' @param env environment donde "viven" los objetos que serán
#'     almacenados; por omisión, el parent.frame
#' @return character: los nombres de los objetos agregados o
#'     \code{NULL} si algún error
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, vi, file = fi)
#'
#' rd <- add_tof(d2, file = fi)
#' list_off(fi, meta = FALSE)
#'
#' unlink(fi)
#' rm(rd, fi, df, d2, vi)
add_tof <- function(..., file, env = parent.frame()) {
    if (!is.environment(env)) {
        message("... environment no existe !!!")
        return(NULL)
    }

    if (missing(...)) {
        message("... nada para agregar !!!")
        return(NULL)
    }

    ## objects' names
    nm <- dots_arg(...)

    ## which objects in env
    jj <- vapply(nm, exists, FALSE,
        envir = env, inherits = FALSE,
        USE.NAMES = FALSE
    )
    if (!all(jj)) {
        warning("... algunos objetos no existen en el environment !!!")
        nm <- nm[jj]
    }

    if (ok <- filled(nm)) {
        ## exists load and process
        if (file.exists(file)) {
            ne <- new.env(parent = emptyenv())
            oo <- try_load(file, ne)

            if (ok <- !is.null(oo)) {
                ## move the objects to the environment where
                ## the objects already in file are bounded
                ob <- mget(nm, env,
                    ifnotfound = vector("list", 1),
                    inherits = FALSE
                )

                ## check for copies
                if (any(copi <- (nm %in% oo))) {
                    message(
                        nm[copi],
                        "\n... remplazar porque mismo nombre !!!"
                    )
                }

                for (jj in seq_along(nm)) {
                    assign(nm[jj], ob[[jj]], envir = ne, inherits = FALSE)
                }

                ok <- save_ok(
                    list = ls(ne, all.names = TRUE),
                    file = file, envir = ne
                )
            }
        } else { # creates new file
            ok <- save_ok(nm, file = file, envir = env)
        }
    }

    if (!ok) {
        nm <- NULL
    }
    return(nm)
}

#' Agregar objetos
#' @description Alias de \code{add_tof}.
#' @param ... nombres de los objetos o un vector tipo character con
#'     los nombres de los objetos que serán agregados
#' @param file character: nombre del archivo. Si no existe, es creado
#' @param env environment donde "viven" los objetos que serán
#'     almacenados; por omisión, el parent.frame
#' @return character: los nombres de los objetos agregados o
#'     \code{NULL} si algún error
#' @seealso \code{add_tof}
#' @export
save_add <- function(..., file, env = parent.frame()) {
    add_tof(..., file, env)
}

#' Guardar data.frame
#' @export
save_df <- function(x, ...) UseMethod("save_df")

#' Almacenar data.frame
#' @description Agrega un data.frame a un archivo, con el mismo u otro
#'     nombre. Si en el archivo hay almacenado un objeto con nombre
#'     igual, ese objeto será remplazado por el data.frame.
#' @param x nombre del data.frame
#' @param name character: nombre con el que será almacenado el
#'     data.frame. Si ausente, se guarda con el mismo nombre.
#' @param file character: nombre del archivo.
#' @param metadata character: breve descripción o metadatos del
#'     data.frame, si es que este no tiene una. Vea la función
#'     \code{meta}
#' @return character: nombre con el que es almacenado el data.frame, o
#'     \code{NULL} si falla la operación
#' @seealso \code{meta}, \code{add_tof}, \code{save_add}
#' @export
#' @examples
#' df <- data.frame(x = 1:3, y = 3:1)
#' meta(df) <- "some metadata"
#' vi <- 1:3
#' fi <- tempfile()
#' save(df, vi, file = fi)
#'
#' d2 <- data.frame(a = letters[1:3], b = LETTERS[1:3])
#' save_df(d2,
#'     name = "first3", file = fi,
#'     metadata = "(lower)uppercase first letters of the alphabet"
#' )
#' list_dff(file = fi)
#'
#' unlink(fi)
#' rm(df, vi, d2, fi)
#' @author eddy castellón
save_df.data.frame <- function(x, name = character(), file,
                               metadata = character()) {
    if (filled_char(metadata)) {
        meta(x) <- metadata
    }

    ok <- is_scalar_name(name)
    ## new name valid?
    ## !!! reserved words?
    ## ok <- filled_char(name) && nzchar(name) &&
    ##     grepl("^[a-zA-z][[:alnum:]]*$", name)
    if (!ok) {
        stop("\n... nombre inválido")
    }

    env <- new.env(parent = emptyenv())
    if (file.exists(file)) { # file.access?
        load(file, envir = env)
    }
    assign(name, x, pos = env)

    if (save_ok(
        list = ls(name = env), file = file, envir = env,
        compress = TRUE
    )) {
        message("\n... d.f ", name, " agregado a ", file)
    } else {
        name <- NULL
    }

    invisible(name)
}

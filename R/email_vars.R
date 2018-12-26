
#' Title Extract features form email address, name and surname
#'
#' @param x - a data.frame object with email address, name and surname.
#' @param address - column's name with email address
#' @param name - column's name with email's owner name
#' @param surname - column's name with of email's owner surname
#'
#' @return
#' @export
#'
#' @examples
#'
email_vars <- function(x, address, name, surname) {

  vars <- c(address, name, surname)
  x <- x[, vars]

  # checking condtitions

  if (!is.data.frame(x))
    stop("object x must be a data.frame!")

  if (!all(sapply(x, class) %in% c("factor", "character")))
    stop("Variables must be factors or characters!")

  if (!all(grepl("^[[:alnum:].-_]+@[[:alnum:].-]+$", x[,address])))
    warning("Your data contains not validated emails!")

  # converting factors to characters

  x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)

  # converting name and surname to LATIN-ASCII and trim

  x[, name] <- stringi::stri_trans_general(x[, name], "LATIN-ASCII")
  x[, surname] <- stringi::stri_trans_general(x[,surname], "LATIN-ASCII")
  x[, name] <- stringi::stri_trim_both(x[, name])
  x[, surname] <- stringi::stri_trim_both(x[, surname])

  # making features based on email, name and surname

  # output<- data.frame(address = x[ , address],
  #                     email_name = sapply(x[, address],, FUN = grepl,  pattern = x[, name], ignore.case = TRUE)
  #                     # email_surname = grepl(x[, surname], x[, address], ignore.case = TRUE),
  #                     # email_both = grepl(x[, name], x[, address], ignore.case = TRUE) &
  #                     #   grepl(x[, surname], x[, address], ignore.case = TRUE)
  #                     )

  stri_opts <- stri_opts_fixed(case_insensitive = TRUE)

  output<- data.frame(address = x[ , address],
                      email_name = stringi::stri_detect_fixed(x[, address], x[, name], opts_fixed = stri_opts),
                      email_surname = stringi::stri_detect_fixed(x[, address], x[, surname], opts_fixed = stri_opts),
                      email_both = stringi::stri_detect_fixed(x[, address], x[, name], opts_fixed = stri_opts) &
                                   stringi::stri_detect_fixed(x[, address], x[, surname], opts_fixed = stri_opts)
  )
  return(output)

}

#' Notice running r code ends.
#'
#' @param expr any syntactically valid \code{R} expression
#' @param overtime threshold
#' @param msg oracle messsage by \code{R} (string)
#' @import after
#' @importFrom remoji emoji
#' @export
#' @examples
#' \dontrun{
#' notice_overtime(1 + 1) # None
#' notice_overtime(for(i in 1:1e7) sqrt(1:10)) # Showing messages.
#' }
notice_overtime <- function(expr, overtime = 0.1, msg = NULL) {

  if (missing(msg)) {
    msg <- paste(remoji::emoji("100"), "Done", remoji::emoji("+1"))
  }

  # calculate running time
  time <- proc.time()
  expr
  new.time <- proc.time()
  fin <- new.time[1] - time[1]

  if (fin < overtime) {
    return(expr)
  } else {
    after::after(2, fun = notice_msg, args = list(msg = msg))
    return(expr)
  }
}

#' Notice running r code evaluate status.
#'
#' @param expr any syntactically valid \code{R} expression
#' @param msg  oracle messsage by \code{R} (string)
#' @param timing when display oracle messsage after evaluation.
#' @import after
#' @import evaluate
#' @import magrittr
#' @importFrom remoji emoji
#' @export
#' @examples
#' \dontrun{
#' notice_status(head(iris))
#'
#' # With chunk statement
#' mtcars %>% dplyr::select(mpg, cyl, disp) %>%
#' dplyr::mutate(disp2 = disp / 2) %>%
#'   head() %>%
#'     notice_status()
#'
#' plot(iris$Sepal.Length, iris$Petal.Width) %>%
#'   notice_status(msg = "DONE!")
#'
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Petal.Width)) +
#' geom_point() %>%
#' notice_status()
#' }
notice_status <- function(expr = NULL, msg = NULL, timing = 2) {

  if (missing(msg)) {
    msg <- paste(remoji::emoji("100"), "Done", remoji::emoji("+1"))
  }

  eval.out <- evaluate::evaluate(quote(expr)) %>%
    magrittr::extract2(2)

  eval.error <- evaluate::is.error(eval.out)
  eval.warng <- evaluate::is.warning(eval.out)
  eval.messg <- evaluate::is.message(eval.out)

  if (eval.error == TRUE) {
    after::after(timing, fun = notice_msg, args = list(msg = paste(remoji::emoji("x"), "Error", remoji::emoji("no_entry_sign"))))
  } else if (eval.warng == TRUE) {
    after::after(timing, fun = notice_msg, args = list(msg = paste(remoji::emoji("warning"), "Warning", remoji::emoji("construction"))))
  } else if (eval.messg == TRUE) {
    after::after(timing, fun = notice_msg, args = list(msg = paste(remoji::emoji("heavy_check_mark"), "Message", remoji::emoji("bulb"))))
  } else {
      after::after(timing, fun = notice_msg, args = list(msg = msg))
  }

  return(expr)
}
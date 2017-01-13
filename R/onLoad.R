#' Sample message
#'
#' @param msg oracle messsage by \code{R} (string)
#' @importFrom remoji emoji
#' @examples
#' \dontrun{
#' notice_msg()
#' }
#' @export
notice_msg <- function(msg = paste(remoji::emoji("100"), "Done", remoji::emoji("+1"))) {
  system(paste0("noti -title 'R' -message '", msg, "'", " echo 'done'"),
         ignore.stdout = TRUE, ignore.stderr = TRUE, wait = TRUE)
}

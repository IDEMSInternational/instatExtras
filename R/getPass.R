#' Password Input (from `getPass` v0.2.2)
#'
#' A password reader function adapted from version 0.2.2 of the 
#' \strong{getPass} package. Like R's \code{readline()}, but the 
#' user-typed input text is not printed to the screen.
#' 
#' @details
#' This function is based on the implementation from getPass 0.2.2, 
#' originally authored by Drew Schmidt. It provides a simple way to 
#' securely capture user input without displaying it on the screen.
#' 
#' Masking (i.e., not displaying the literal typed text as input) is supported 
#' on most, but not all platforms. It is supported in RStudio, provided you 
#' have a suitable version of the GUI. It should also work in the terminal 
#' on any major OS. Additionally, it will work in any environment where 
#' the tcltk package is available (e.g., Windows with RGui). 
#' Notably, this will not work with Emacs; passwords can be read, but 
#' there will be no masking.
#' 
#' In the terminal, the maximum length for input is 255 characters. 
#' Additionally, messages printed to the terminal (including the "*" masking) 
#' are printed to stderr.
#' 
#' @param msg
#' The message to enter into the R session before prompting
#' for the masked input. This can be any single string, possibly including 
#' a "blank" (\code{""}); see the \code{noblank} argument.
#' @param noblank
#' Logical; should blank passwords (\code{""}) be banned? By default, 
#' they are allowed, except with RStudio where they are always banned.
#' @param forcemask
#' Logical; should the function stop with an error if masking
#' is not supported? If \code{FALSE}, the function will default
#' to use \code{readline()} with a warning message that the
#' input is not masked, and otherwise will stop with an error.
#' See the Details section for more information.
#' 
#' @return
#' If input is provided, then that is returned. If the user cancels
#' (e.g., cancel button on RStudio or ctrl+c in the terminal), then
#' \code{NULL} is returned.
#' 
#' @examples
#' \dontrun{
#' # Basic usage
#' getPass::getPass()
#' 
#' # Get password with a custom message
#' getPass::getPass("Enter the password: ")
#' }
#' 
#' @references
#' Schmidt, D. (2017). \emph{Guide to the getPass Package}. 
#' R package version 0.2.2. Retrieved from \url{https://CRAN.R-project.org/package=getPass}
#' 
#' @note
#' This function is directly adapted from version 0.2.2 of the `getPass` package. 
#' Full credit to Drew Schmidt for the original implementation.
#' 
#' @export
getPass <- function(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE) {
  if (!is.character(msg) || length(msg) != 1 || is.na(msg)) {
    stop("argument 'msg' must be a single string")
  }
  if (!is.logical(noblank) || length(noblank) != 1 || is.na(noblank)) {
    stop("argument 'noblank' must be one of 'TRUE' or 'FALSE'")
  }
  if (!is.logical(forcemask) || length(forcemask) != 1 || is.na(forcemask)) {
    stop("argument 'forcemask' must be one of 'TRUE' or 'FALSE'")
  }
  
  if (tolower(.Platform$GUI) == "rstudio") pw <- readline_masked_rstudio(msg = msg, noblank = noblank, forcemask = forcemask)
  #else if (isaterm()) pw <- readline_masked_term(msg = msg, showstars = TRUE, noblank = noblank)
  else if (hastcltk()) pw <- readline_masked_tcltk(msg = msg, noblank = noblank)
  else if (!forcemask) pw <- getPass_readline(msg)
  else stop("Masking is not supported on your platform!")
  
  if (is.null(pw)) invisible()
  else pw
}

getPass_readline <- function(...) {
  readline(...)
}

# other functions used in this
readline_masked_rstudio <- function(msg, forcemask, noblank=FALSE){
  if (noblank){
    while (TRUE){
      pw <- readline_masked_rstudio_window(msg, forcemask)
      if (is.null(pw) || pw != "")
        break
    }
  } else{
    pw <- readline_masked_rstudio_window(msg, forcemask)
  }
  return(pw)
}

# isaterm <- function(){
#   gui <- .Platform$GUI
#   
#   if (!isatty(stdin())) return(FALSE)
#   # ban emacs: here and everywhere else in life
#   else if (Sys.getenv("EMACS") == "t" || identical(getOption("STERM"), "iESS")) return(FALSE)
#   else if (gui == "RTerm" || gui == "X11") return(TRUE)
#   else if (gui == "unknown" && .Platform$OS.type == "unix" && Sys.getenv("RSTUDIO") != 1 && Sys.getenv("R_GUI_APP_VERSION") == "") return(TRUE) # I think?
#   else return(FALSE)
# }

hastcltk <- function(){
  test <- tryCatch(requireNamespace("tcltk", quietly=TRUE), warning=identity)
  if (!is.logical(test)) test <- FALSE
  test
}

os_windows = function(){
  .Platform$OS.type == tolower("windows")
}

readline_masked_rstudio_window <- function(msg, forcemask){
  if (!has_fun("askForPassword")){
    stopmsg <- "Masked input is not supported in your version of RStudio; please update to version >= 0.99.879"
    if (!forcemask){
      message(paste0("NOTE: ", stopmsg, "\n"))
      pw <- readline_nomask(msg, silent=TRUE)
    } else {
      stop(stopmsg)
    }
  } else {
    pw <- ask_for_password(msg)
  }
  return(pw)
}

readline_nomask <- function(msg, noblank = TRUE, silent=FALSE){
  if (!silent) message("WARNING: your platform is not supported. Input is not masked!")
  message(msg, appendLF=FALSE)
  pw <- readline()
  while (interactive() && isTRUE(noblank) && pw == "") {
    message("No blank input, please!", appendLF=FALSE)
    pw <- readline()
  }
  return(pw)
}

readline_masked_tcltk <- function(msg, noblank=FALSE){
  msg.console <- "Please enter password in TK window (Alt+Tab)\n"
  cat(msg.console)
  utils::flush.console()
  readline_masked_tcltk_window(msg, noblank)
}

readline_masked_tcltk_window <- function(msg, noblank = FALSE, test_mode = NULL) {
  # Add noblank to msg
  if (noblank) msg <- paste0(msg, "\n(no blank)")
  
  # Define event actions
  tcreset <- function(){ 
    tcltk::tclvalue(pwdvar) <- ""
  }
  
  tcsubmit <- function(){
    if (noblank && tcltk::tclvalue(pwdvar) == ""){
      tcltk::tkmessageBox(title = "getPass noblank = TRUE",
                          message = "No blank input please!",
                          parent = textbox)
    } else {
      tcltk::tclvalue(flagvar) <- 1
      tcltk::tkdestroy(tt)
    }
  }
  
  tccleanup <- function(){
    tcltk::tclvalue(flagvar) <- 0
    tcltk::tkdestroy(tt)
  }
  
  if (!is.null(test_mode)) {
    pwdvar <- "test"
    tcreset()
    tcsubmit()
    tccleanup()
    return(test_mode)  # Return the test value if test_mode is set
  }
  
  # Main window
  tt <- tcltk::tktoplevel()
  tcltk::tktitle(tt) <- "getPass input"
  pwdvar <- tcltk::tclVar("")
  flagvar <- tcltk::tclVar(0)
  
  # Main frame
  f1 <- tcltk::tkframe(tt)
  tcltk::tkpack(f1, side = "top")
  tcltk::tkpack(tcltk::tklabel(f1, text = msg), side = "left")
  
  # Main entry
  textbox <- tcltk::tkentry(f1, textvariable = pwdvar, show = "*")
  tcltk::tkpack(textbox, side = "left")
  tcltk::tkbind(textbox, "<Return>", tcsubmit)
  if (.Platform$OS.type == "windows") tcltk::tkbind(textbox, "<Escape>", tccleanup)
  else tcltk::tkbind(textbox, "<Control-c>", tccleanup)
  
  # Buttons for submit and reset
  reset.but <- tcltk::tkbutton(f1, text = "Reset", command = tcreset)
  submit.but <- tcltk::tkbutton(f1, text = "Submit", command = tcsubmit)
  
  tcltk::tkpack(reset.but, side = "left")
  tcltk::tkpack(submit.but, side = "right")
  
  # Add focus
  tcltk::tkwm.minsize(tt, "320", "40")
  tcltk::tkwm.deiconify(tt)
  tcltk::tkfocus(textbox)
  
  # Wait for destroy signal
  tcltk::tkwait.window(tt)
  pw <- tcltk::tclvalue(pwdvar)
  
  # Check for return
  flag <- tcltk::tclvalue(flagvar)
  if (flag == 0) pw <- NULL
  
  return(pw)
}

# readline_masked_term <- function(msg, showstars, noblank=FALSE){
#   .Call(getPass_readline_masked, msg, as.integer(showstars), as.integer(noblank))
# }

ask_for_password <- function(msg){
  rstudioapi::askForPassword(prompt = msg)
}

has_fun <- function(name){
  rstudioapi::hasFun(name)
}
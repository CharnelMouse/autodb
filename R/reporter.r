# utility function for printing optional progress messages without obfuscating
# program flow. op(eration) is designed to work in a pipe. exp(ression) is
# designed to work on a single statement, often an assignment. stat(ement) just
# prints the message if needed, and is otherwise a no-op.
reporter <- function(report, con, new = FALSE) {
  force(new)
  if (report)
    function(text) {
      cat(paste0(text, "\n"), file = con, append = !new)
      utils::flush.console()
      new <<- FALSE
      invisible(NULL)
    }
  else
    function(text) invisible(NULL)
}

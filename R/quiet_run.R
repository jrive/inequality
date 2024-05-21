quiet_run <- function(expr) {
  # Create temporary files to capture output and messages
  temp_output <- tempfile()
  temp_message <- tempfile()
  
  # Open connections to the temporary files
  output_conn <- file(temp_output, open = "wt")
  message_conn <- file(temp_message, open = "wt")
  
  # Redirect all output and messages to the temporary connections
  sink(output_conn)
  sink(message_conn, type = "message")
  
  # Ensure normal output is restored after the function execution
  on.exit({
    sink()  # Restore output
    sink(type = "message")  # Restore messages
    close(output_conn)
    close(message_conn)
    unlink(temp_output)
    unlink(temp_message)
  }, add = TRUE)
  
  # Capture warnings and handle them
  result <- withCallingHandlers(
    tryCatch(
      suppressMessages(suppressWarnings(expr)),
      error = function(e) e
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )
  
  return(result)
}

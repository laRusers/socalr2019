condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}

malformed_log_entry_error <- function(text) {
  text <- 'my test text'
  msg <- paste0("Malformed log entry: ", text)
  condition(c("malformed_log_entry_error"),
            message = msg
  )
}

well_formed_log_entry <- function(text) {
  length(text) == 1 && length(strsplit(text, ',')[[1]]) == 25
  runif(1) > 0.5
}

new_log_entry <- function(text) {
  text
}

parse_log_entry <- function(text) {
  if (!well_formed_log_entry(text)) {
    signalCondition(malformed_log_entry_error(text))
  }

  new_log_entry(text)
}

parse_log_file <- function(file) {
  lines <- readLines(file)

  lapply(lines, function(text) {
    withRestarts(
      parse_log_entry(text),
#      skip_log_entry = function(e) NULL,
      mock_log_entry = function(e) '"OCRUG Data Science Hackathon","1234567890","11/8/19","University of California Irvine","39324297","35.00","32.63","1.49","0.88","0.00","1","Credit/Debit Card","Eventbrite Completed","","eTicket","Visa - XXXX-XXXXXX-2844","Mohan","Ganesan","ganesan.mohan1@gmail.com","","","","","92692","US"'
    )
  })
}

find_all_logs <- function() {
  'order.csv'
}

skip_log_entry_handler <- function(cond) {
  r <- findRestart("skip_log_entry")
  if (is.null(r)) {
    print('No skip_log_entry found')
    return()
  }

  print("found skip_log_entry")
  invokeRestart(r)
}

mock_log_entry_handler <- function(cond) {
  r <- findRestart("mock_log_entry")
  if (is.null(r)) return()

  invokeRestart(r)
}


analyze_entry <- function(text) {
  strsplit(text, ',')[[1]][2] %>%
    stringr::str_replace_all('\"', "") %>%
    stringr::str_replace('^"', '') %>%
    stringr::str_replace('"$', '')
}


analyze_log <- function(log) {
  entries <- parse_log_file(log)
  lapply(entries, analyze_entry)
}


log_analyzer <- function() {
  logs <- find_all_logs()

  withCallingHandlers(
    lapply(logs, analyze_log) %>% unlist(),
    malformed_log_entry_error = skip_log_entry_handler,
    malformed_log_entry_errorX = mock_log_entry_handler
  )
}


log_analyzer()

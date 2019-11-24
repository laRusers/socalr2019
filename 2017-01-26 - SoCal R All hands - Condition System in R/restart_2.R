rm(list=ls())

condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}

skip_entry_condition <- function(text, call = sys.call(-1), ...) {
  condition(c("skip_entry"),
            message = paste0("Skipping entry: ", text),
            call = call,
            text = text,
            ...
  )
}

skip_entry_handler <- function(cond) {
  r <- findRestart("skip_entry")
  if (is.null(r)) { return() }

  invokeRestart(r)
}

skip_entry_test <- function(text) {
  as.logical(rbinom(1, 1, 0.5))
}

parse_entry <- function(text) {
  if (!skip_entry_test(text)) {
    signalCondition(skip_entry_condition(text))
  }

  strsplit(text, ',') %>% unlist()
}


parse_file <- function(file) {
  lines <- readLines(file)

  lapply(lines, function(text) {
    withRestarts(
      parse_entry(text),
      skip_entry = function(e) NULL
    )
  }) %>%
    plyr::compact()
}


random_sample <- function(file) {
  withCallingHandlers(
    parse_file(file),
    skip_entry = skip_entry_handler
  )
}


a <- random_sample('hackathon.csv')

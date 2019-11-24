rm(list=ls())

library(magrittr)
library(plyr)


condition <- function(condition_class, message, call = sys.call(-1), ...) {
  structure(
    class = c(condition_class, "condition"),
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


keep_entry_condition <- function(text, call = sys.call(-1), ...) {
  condition(c("keep_entry"),
            message = paste0("Keeping entry: ", text),
            call = call,
            text = text,
            ...
  )
}

condition_handler_factory <- function(restart_name) {
  function(cond) {
    restart <- findRestart(restart_name)
    if (is.null(restart)) { return() }

    invokeRestart(restart)
  }
}

keep_entry_handler <- condition_handler_factory('keep_entry')
skip_entry_handler <- condition_handler_factory('skip_entry')

choose_entry_test <- function(text, p_skip){

  if (as.logical(rbinom(1, 1, p_skip))) {
    signalCondition(skip_entry_condition(text))
  }

  signalCondition(keep_entry_condition(text))

}


parse_entry <- function(text) {
  strsplit(text, ',') %>% unlist()
}


parse_file <- function(file) {
  lines <- readLines(file)

  lapply(lines, function(text) {
    withRestarts(
      choose_entry_test(text, p_skip = 0.5),
      skip_entry = function() NULL,
      keep_entry = function() parse_entry(text)
    )
  }) %>%
    plyr::compact()
}


random_sample <- function(file) {
  withCallingHandlers(
    parse_file(file),
    skip_entry = skip_entry_handler,
    keep_entry = keep_entry_handler
  )
}


a <- random_sample('order_zip.csv')

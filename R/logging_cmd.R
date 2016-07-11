
printLine <- function(...) {
  cat(..., '\n')
}

constructLogger <- function(loggingFunction) {
  function(...) {
    args <- as.character(unlist(list(...)))
    printLine(loggingFunction(args))
  }
}

blueInfo <- constructLogger(blue)
greenInfo <- constructLogger(green)
yellowInfo <- constructLogger(yellow)
warning <- constructLogger(function(...) { red("!", ...) })


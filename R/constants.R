### Package constants and cache

library(magrittr)

# API URL ---------------------------------------------------------------------

WRITTEN_QUESTIONS <- "https://writtenquestions-api.parliament.uk/api/writtenquestions/questions"

# Cache -----------------------------------------------------------------------

cache <- new.env(parent = emptyenv())

#'@export

getValues <- function(name, flags, ext){
  name <- sub(pattern = ext, replacement = "", x = name)
  vals <- strsplit(name, split = "_")[[1]]
  values <- sapply(flags, function(x){
    value <- vals[grepl(pattern = paste0("^", x, "-"), x = vals)]
    value <- sub(pattern = paste0(x, "-"), replacement = "", value)
  })
  values
}

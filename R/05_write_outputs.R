# Output tables

write_table <- function(x, path) {
  write.csv(x, path, row.names = FALSE)
}

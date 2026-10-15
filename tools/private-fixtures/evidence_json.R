# Preserve floating-point evidence through JSON, including support-boundary axes.
# Original private receipts are historical records and are never rewritten here.
write_evidence_json <- function(value, path) {
  jsonlite::write_json(value, path, auto_unbox = TRUE, digits = 17)
}

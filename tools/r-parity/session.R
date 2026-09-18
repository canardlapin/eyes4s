# The pinned R session of every eyes4s parity generator.
#
# parity.py starts each generator script as
#   Rscript --no-save --no-restore --no-site-file --no-environ <script> <args>
# with R_PROFILE_USER naming this file, so it runs before the script in place of any
# user profile. It fixes whatever in the session could change a fixture: the package
# library, the printing, quoting and warning options, the RNG kinds and the locale.
# A script that needs another value sets it explicitly after this file has run.
local({
  lib <- Sys.getenv("EYES4S_R_LIBRARY")
  if (!nzchar(lib) || !dir.exists(lib)) {
    stop("EYES4S_R_LIBRARY must name the provisioned library; run tools/r-parity/provision.py")
  }
  # R always keeps .Library itself; the user and site libraries are dropped.
  .libPaths(lib)
  options(
    digits = 7L, scipen = 0L, OutDec = ".", width = 80L, warn = 0L,
    warning.length = 1000L, nwarnings = 50L, useFancyQuotes = FALSE,
    keep.source = FALSE, encoding = "native.enc"
  )
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  for (category in c("LC_COLLATE", "LC_CTYPE", "LC_NUMERIC")) {
    if (!identical(Sys.getlocale(category), "C")) stop(category, " must be C; run through parity.py")
  }
})

# Record what the script loaded, and from where, for parity.py to check against r-lock.json.
# .Last runs only when the script completes; a failed script leaves no record. The
# path is taken out of the environment so that child R processes, which also start
# with this profile, cannot overwrite the record.
.Last <- local({
  audit <- Sys.getenv("EYES4S_R_AUDIT")
  Sys.unsetenv("EYES4S_R_AUDIT")
  function() {
    if (!nzchar(audit)) return(invisible())
    names <- sort(loadedNamespaces())
    home <- function(name) {
      dirname(if (name == "base") system.file(package = "base") else getNamespaceInfo(name, "path"))
    }
    rows <- vapply(names, function(name) {
      paste(name, getNamespaceVersion(name), home(name), sep = "\t")
    }, character(1))
    writeLines(c(paste("R", as.character(getRversion()), sep = "\t"), rows), audit)
  }
})

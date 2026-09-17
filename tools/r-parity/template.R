# Public eyesim map regression is a distinct estimand from held-out trial prediction.
args <- commandArgs(trailingOnly = TRUE)
.libPaths(c(args[[1L]], .libPaths()))
library(eyesim)
input <- jsonlite::fromJSON(args[[2L]])$template_model
# jsonlite simplifies the fixed feature arrays to a matrix on some versions.
x <- if (is.matrix(input$training$features)) input$training$features else
  do.call(rbind, input$training$features)
y <- input$training$response
maps <- tibble::tibble(
  response = list(list(z = matrix(y, nrow = 2))),
  template_a = list(list(z = matrix(x[, 1], nrow = 2))),
  template_b = list(list(z = matrix(x[, 2], nrow = 2))))
fit <- eyesim::template_multireg(maps, "response", c("template_a", "template_b"),
                                method = "lm", intercept = FALSE)$multireg[[1]]
out <- list(terms = fit$term, coefficients = fit$estimate,
            R = as.character(getRversion()),
            packages = setNames(lapply(sort(loadedNamespaces()),
              function(p) as.character(utils::packageVersion(p))), sort(loadedNamespaces())))
jsonlite::write_json(out, args[[3L]], pretty = TRUE, auto_unbox = TRUE, digits = NA)

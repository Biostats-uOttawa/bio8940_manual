#!/usr/bin/env Rscript

# use only if renv is activated (not activated now)
# renv::hydrate()
# renv::refresh()
# renv::snapshot()


knitr::write_bib(.packages(TRUE), "packages.bib")


rm(list = ls())
bookdown::render_book(
    "index.Rmd",
    "bookdown::gitbook",
    params = list(echo_sol = TRUE, html_pdf = TRUE)
)
rm(list = ls())
bookdown::render_book(
    "index.Rmd",
    "bookdown::pdf_book",
    params = list(echo_sol = FALSE, html_pdf = FALSE)
)
rm(list = ls())
bookdown::render_book(
  "index.Rmd", "bookdown::epub_book",
  params = list(echo_sol = FALSE, html_pdf = TRUE)
)


bookdown::preview_chapter("02_02-intro_lmm.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("02_03-intro_glmm.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("02_04-intro_bayesian.Rmd",
  output_dir = "docs/preview")
bookdown::preview_chapter("04-t_test.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("05-anova.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("06-anova_mult.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("07-reg_mult.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("08-ancova_glm.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("09-model_freq.Rmd", output_dir = "docs/preview")

purl_fn <- function(chapter) {
    for (i in chapter) {
        knitr::purl(
            paste0(i, ".Rmd"),
            output = paste0("../practical_data/R_code_chapter/", i, ".R"))
    }
}
purl_fn(c(
    "01_03-github",
    "02_01-intro_glm",
    "02_02-intro_lmm",
    "02_03-intro_glmm",
    "02_04-intro_bayesian",
    "02_05-multivar",
    "02_06-randreg"
))

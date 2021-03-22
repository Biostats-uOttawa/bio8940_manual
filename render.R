#!/usr/bin/env Rscript

renv::hydrate()
renv::refresh()
renv::snapshot()
knitr::write_bib(c(
    .packages(), rownames(installed.packages())
    # "bookdown", "knitr", "rmarkdown", "MCMCglmm", "gremlin"
), "packages.bib")


rm(list = ls())
bookdown::render_book("index.Rmd", "bookdown::gitbook", params = list(echo_sol = TRUE))
rm(list = ls())
bookdown::render_book("index.Rmd", "bookdown::pdf_book", params = list(echo_sol = FALSE))
rm(list = ls())
bookdown::render_book("index.Rmd", "bookdown::epub_book")
bookdown::calibre("docs/BIO8940_book.epub", "mobi")


# bookdown::preview_chapter("06-anova_mult.Rmd", output_format = 'bookdown::html_chapters', output_dir = "docs/preview")
bookdown::preview_chapter("02_02-intro_lmm.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("02_03-intro_glmm.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("02_04-intro_bayesian.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("04-t_test.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("05-anova.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("06-anova_mult.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("07-reg_mult.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("08-ancova_glm.Rmd", output_dir = "docs/preview")
bookdown::preview_chapter("09-model_freq.Rmd", output_dir = "docs/preview")

knitr::purl("02_02-intro_lmm.Rmd")
knitr::purl("02_03-intro_glmm.Rmd")

#!/bin/sh

Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook', params = list(echo_sol = TRUE, html_pdf = TRUE))"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book', params = list(echo_sol = FALSE, html_pdf = FALSE))"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown:::epub_book', params = list(echo_sol = FALSE, html_pdf = TRUE))"
Rscript -e "bookdown::calibre('docs/BIO8940_book.epub', 'mobi')"

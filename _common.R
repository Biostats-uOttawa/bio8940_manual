knitr::opts_chunk$set(echo = TRUE, cache = TRUE)

params <- list(
   echo_sol = TRUE,
   longrun = FALSE,
   html_pdf = TRUE
)

htmlT_pdfF <- ifelse(params$html_pdf == TRUE, TRUE, FALSE)
htmlF_pdfT <- ifelse(params$html_pdf == TRUE, FALSE, TRUE)

library(bookdown)
library(lmerTest)
library(rptR)
library(performance)
library(MCMCglmm)
library(nadiv)
library(brms)
library(lattice)
library(palmerpenguins)
library(factoextra)
library(tidybayes)
library(tidyverse)
library(bayesplot)
library(patchwork)


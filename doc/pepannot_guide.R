## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Loading the pepannot package
library(pepannot)

## -----------------------------------------------------------------------------
# Loading additional required packages
library(readr)
library(dplyr)

## -----------------------------------------------------------------------------
# Read the CSV file containing peptide data
b <- readr::read_csv(system.file("extdata", "anon.csv", package = "pepannot"))

## -----------------------------------------------------------------------------
# Annotate peptide sequences with modifications
c <- b |>
  dplyr::rowwise() |>
  dplyr::mutate(new_column_name = annotate_positions(seq, residue_posn)) |>
  dplyr::mutate(another_column_name = annot_sequence(seq, residue_posn, pattern))


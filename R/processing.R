# Copyright 2024 Alexandre Ramos, AMPhyBio Laboratory <alex.ramos@usp.br>
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, you can obtain one at https://mozilla.org/MPL/2.0/
#
# Project:  AMPhyBio Utilities
# Version:  0.1.0
# Created:  24-10-2024
# Authors:
#   - Leonardo Gama <leonardo.gama@usp.br> <leogama@github>


combat <- function(
        data,
        batch,
        vars = NULL,
        covars = NULL,
        parametric = TRUE,
        mean.only = FALSE,
        ref.batch = NULL,
        ...) {

    #'  Apply the ComBat batch effect correction method to tabular data.
    #'
    #'  Convenience function to apply `sva::ComBat()` to data in the
    #'  common `data.frame` format and with less clunky parameters.
    #'
    #'  Parameters:
    #'      data (data.frame): Containing variables to be corrected.
    #'      batch (character(1)|factor):
    #'          Either the name of the column with batch information or
    #'          a vector the same length as the number of rows in 'data'
    #'          containing this information (categorical).
    #'      vars (character):
    #'          Names of the columns to correct for batch effect. If
    #'          `NULL`, the default, all numeric columns except those
    #'          listed in 'batch' and 'covars' are transformed.
    #'      covars (formula):
    #'          Formula with the outcome of interest variable and other
    #'          possible covariates besides batch. Example:
    #'          `~ outcome + covar1 + covar2`
    #'      parametrict (logical(1)):
    #'          If `TRUE` (default) use the parametric version of the
    #'          ComBat method, which assumes normality of data.
    #'      mean.only (logical(1):
    #'          If TRUE ComBat only corrects the mean of the batch
    #'          effect (no scale adjustment)
    #'      ref.batch: (character(1)):
    #'          If given, will use the selected batch as a reference for
    #'          batch adjustment.
    #'      ...: Extra arguments passed to `sva::ComBat()`.
    #'
    #'  Returns:
    #'      data.frame: the data corrected for batch effect.
    #'
    #'  See also:
    #'      `sva::ComBat()`
    #'
    #'  @export

    if (!requireNamespace('sva', quietly = TRUE)) {
        stop("Package 'sva' is not installed but is required.")
    }

    if (missing(vars)) {
        num_data <- data |> select(where(is.numeric), -all_of(covars)) 
        if (length(batch) == 1) {
            num_data <- num_data |> select(-any_of(batch))
        }
    } else {
        num_data <- data |> select(all_of(vars))
    }

    if (length(batch) == 1) {
        batch <- data[[batch]]
    }

    mod <- if (!is.null(covars)) {
        str_c('~', str_c(covars, collapse = '+')) |>
            formula() |>
            model.matrix(data)
    }

    # NOTE: The original function was developed for gene expression data
    # and expects the input in a transposed form (variables in rows and
    # samples in columns).
    num_data <- t(num_data)
    res <- suppressMessages(sva::ComBat(
        num_data,
        batch = batch,
        mod = mod,
        par.prior = parametric,
        mean.only = mean.only,
        ref.batch = ref.batch,
        ...)) |>
    t() |>
    as.data.frame()

    data |> mutate(res)
}

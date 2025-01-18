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


# Mapping of column position to spreadsheet column name (up to "ZZ").
HEADER_NAME <- expand.grid(LETTERS, c('', LETTERS))
HEADER_NAME <- str_c(HEADER_NAME[[2]], HEADER_NAME[[1]])
HEADER_NAME <- seq_along(HEADER_NAME) |> set_names(HEADER_NAME)


#' Convert spreadsheet column ranges to numeric indexes
#'
#' Para criar os objetos que serão usados nas análises, é preciso
#' selecionar as colunas desejadas, na ordem desejada. Os editores de
#' planilha indicam as colunas usando índices alfabéticos ("C", "AA").
#' Podemos construir um dicionário (baseado em resposta no site
#' StackOverflow).
#'
#' @export

sheet_cols <- function(...) {
    args <- toupper(as.character(list(...)))
    if (length(args) == 1) {
        # Accept indexes as a comma separated list in a character string.
        args <- strsplit(trimws(args), '[[:space:]]*,[[:space:]]*')[[1]]
    }
    invalid <- grep('^[A-Z]{1,2}(:[A-Z]{1,2})?$', args, invert = TRUE, value = TRUE)
    if (length(invalid) > 0) {
        stop("Invalid column index or range:  ", paste(invalid, collapse = ", "))
    }

    cols <- integer()
    for (col_index in strsplit(args, ':')) {
        if (length(col_index) == 1) {
            cols <- c(cols, HEADER_NAME[col_index[[1]]])
        } else {
            # Preserve columns' alphabetical indexes in names.
            range_begin <- HEADER_NAME[col_index[[1]]]
            range_end <- HEADER_NAME[col_index[[2]]]
            cols <- c(cols, HEADER_NAME[range_begin:range_end])
        }
    }
    cols
}


#' Convert factor to ordered keeping unused levels.
#'
#' By default, ordered() and as.ordered() drop unused levels.
#'
#' @param x  A factor
#' @returns  An ordered factor
#' @seealso [as.ordered()]
#' @export

as_ordered_keep_levels <- function(x) {
    if (is.ordered(x)) {
        x
    } else if (!is.factor(x)) {
        ordered(x)
    } else {
        factor(x, levels = levels(x), ordered = TRUE)
    }
}


default_casting <- list(
    binary = as.logical,
    nominal = as.factor,
    ordinal = as_ordered_keep_levels,
    discrete = as.integer,
    continuous = as.numeric,
    date = as.Date,  # should use POSIX time?
    text = as.character
)


read_sheet <- function(
        path,
        sheet = NULL,
        range = NULL,
        guess_max = 1000,
        check = TRUE,
        checksum = NULL,
        checksum_func = tools::md5sum,
        rows = NULL,
        id_col = NULL,
        id_cast = NULL,
        sensible_cols = NULL,
        hash_func = partial(sapply, FUN = rlang::hash),
        ...) {

    #'  Read an Excel sheet applying some checks.
    #'
    #'  Read a single sheet from a spreadsheet file in Microsoft Excel's format
    #'  into a data.frame using the readxl package.  Check the file existence,
    #'  its checksum, the number of rows expected to be read and...
    #'
    #'  Parameters:
    #'      path (character): Path to the xls/xlsx file.
    #'      sheet (character|integer):
    #'          Sheet to read.  Either a string (the name of a sheet), or an
    #'          integer (the position of the sheet).  Defaults to the first
    #'          sheet in the file
    #'      range (character):
    #'          A cell range to read from, as described in cell-specification.
    #'          Includes typical Excel ranges like "B3:D87" and more.
    #'      guess_max (integer):
    #'          Maximum number of data rows to use for guessing column types.
    #'      check (logical): Whether to apply the checks (see below).
    #'      checksum (character): Checksum string of the file in 'path'.
    #'      rows (integer):
    #'          Expected number of rows in the returned table. If 'id_col' is
    #'          specified, this is the number of rows left after dropping those
    #'          with NA values in the identifier column.
    #'      id_col (character|integer):
    #'          If an integer, the number of the column to be used as
    #'          identifier.  If text, a regular expression that matches the name
    #'          of that column.
    #'      id_cast (function):
    #'          A function applied to the values of the identifier column.
    #'          Example: 'as.integer'.
    #'      sensible_cols (integer|character):
    #'          Numbers or names of columns that contain sensible data and
    #'          should be annonymized by hashing its values with 'hash_func'.
    #'      hash_func (function):
    #'          A function that converts vectors (the columns specifiend by
    #'          'sensible_cols') to vectors of hash strings.
    #'      ...:
    #'          Extra arguments passed to 'readxl::read_excel'.
    #'
    #'  Returns:
    #'      data.frame: The read table (sheet of spreadsheet file).
    #'
    #'  Raises:
    #'      Always stops if:
    #'          - the expression in 'id_col' doesn't match exactly 1 column
    #'      When 'check' is TRUE, stops if:
    #'          - the file pointed by 'path' is not accessible for any reason
    #'          - the 'checksum' value doesn't match the file's checksum
    #'          - the number of rows with valid identifiers isn't equal 'rows'
    #'
    #'  See Also:
    #'      `vignette('sheet-geometry', package = 'readxl')`

    #TODO: write simple examples
    #  Example:
    #
    #      sheets <- readxl::read_excel('data/annotation.xlsx, sheet = 'sheets') |>
    #          column_to_rownames('name')  # also casts to data.frame
    #
    #      raw_dat <- list()
    #
    #      for (i in seq(nrow(sheets))) {
    #          name <- rownames(sheets)[i]
    #          raw_dat[[name]] <- with(sheets[i, ], {
    #              read_sheet(
    #                  path = user::data_file(filename),
    #                  sheet = sheet_name,
    #                  range = range,
    #                  guess_max = 10000,
    #                  checksum = md5sum,
    #                  id_col = id_col,
    #                  id_cast = as.integer,
    #                  sensible_cols = if (!is.na(sensible_cols)) str_split_1(sensible_cols, ';'),
    #                  rows = rows
    #              )
    #          })
    #      }
    #
    #  # Reexport data to tabulated files.
    #  for (name in names(raw_dat)) {
    #      filename <- sprintf('data/%s.tsv', name)
    #      write.table(raw_dat[[name]], filename, sep = '\t', row.names = FALSE)
    #  }

    if (!requireNamespace('readxl', quietly = TRUE)) {
        stop("Package 'readxl' is not installed but is required.")
    }

    is_nil <- function(x) is.null(x) || (length(x) == 1 && is.na(x))

    # Verify data file exists and is readable.
    if (check && file.access(path, mode = 4) != 0) {
        stop(sprintf("file is not accessible: '%s'", path))
    }

    # Check file integrity with checksum.
    if (check && !is_nil(checksum)
            && !identical(checksum, unname(checksum_func(path)))) {
        stop(sprintf("does not match checksum: '%s'", path))
    }

    # Read single sheet.
    cat(sprintf("Reading '%s'\n", path))

    #if (missing(guess_max) && !missing(range) && is.character(range)) {
        #TODO
    #}

    dat <- readxl::read_excel(
            path = path,
            sheet = if (!is_nil(sheet)) sheet,
            range = range,
            guess_max = guess_max,
            ...) |>
        #FIXME: trim_ws from read_excel is ignored (using str_trim)
        mutate(across(where(is.character), str_trim)) |>
        as.data.frame()

    # Remove spurious whitespaces from column headers.
    colnames(dat) <- colnames(dat) |> str_squish()

    # Manipulate the ID column.
    if (!is_nil(id_col)) {
        # Match exact regex to find the ID column.
        if (is.character(id_col)) {
            match_col <- colnames(dat) |>
                str_which(str_c('^', id_col, '$'))

            #TODO: cover all cases
            if (length(match_col) != 1) {
                "the 'id_col' expression matched %d columns: '%s'" |>
                    sprintf(length(match_col), id_col) |>
                    stop()
            }

            id_col <- match_col
        }

        # Relocate and rename the ID column to 'id'.
        dat <- dat |> relocate(id = id_col, .before = 1)

        # Cast id column and drop invalid/empty entries.
        if (!is.null(id_cast)) {
            dat[['id']] <- suppressWarnings(id_cast(dat[['id']]))
        }
        dat <- dat |> filter(!is.na(id))
    }

    # Calculate hash of sensible (unused) data.
    if (!is_nil(sensible_cols)) for (col in sensible_cols) {
        dat[[col]] <- hash_func(dat[[col]])
    }

    # Check if the number of rows is the expected.
    if (check && !is_nil(rows)) {
        stopifnot(nrow(dat) == rows)
    }

    dat
}


## Functions for data extraction, formatting and validation ##

annotation <- attr_getter('annotation')

construct_table <- function(
        table_name,
        origin_tables,
        annot_table,
        check = TRUE,
        casting = NULL,
        na_values = NULL) {

    #'  Generate a tidy (well format) data.frame from one or potentially more
    #'  tabulated file's original data.
    #'
    #'  Long description...
    #'
    #'  Parameters:
    #'      table_name (character):
    #'          The tables's identification in the column named "table" in the
    #'          'annot_table' data.frame.
    #'      origin_tables (list[data.frame]):
    #'          A list containing the data.frames corresponding to each
    #'          tabulated file or sheet of original data.
    #'      annot_table (data.frame): annotation of variables.
    #'      check (logical): whether to perform checks on data.
    #'
    #'  Returns:
    #'      data.frame: The read table (sheet of spreadsheet file).
    #'
    #'  Raises:
    #'      Always stops if:
    #'          - the expression in 'id_col' doesn't match exactly 1 column
    #'      When 'check' is TRUE, stops if:
    #'          - the file pointed by 'path' is not accessible for any reason
    #'          - the 'checksum' value doesn't match the file's checksum
    #'          - the number of rows with valid identifiers isn't equal 'rows'
    #'
    #'  See Also:
    #'      `vignette('sheet-geometry', package = 'readxl')`

    data <- table_name |>
        construct_from_annotation(origin_tables, annot_table) |>
        relabel_factors() |>
        cast_types(casting, na_values) |>
        generate_derived()

    #TODO: implement and test
    #if (check) {
    #    check_types_values(data)
    #}

    data
}


construct_from_annotation <- function(
        table_name,
        origin_tables,
        annot_table,
        col_types = NULL) {

    # Get primary columns for this table.
    primary_columns <- annot_table |>
        filter(table == table_name, origin != 'ALL', origin != 'DERIVED') |>
        mutate(header = coalesce(header, variable))

    #with(primary_columns, print(variable |> set_names(header)))

    # Extract the specified columns from the original tables.
    parts <- list()
    for (origin_table in unique(primary_columns$origin)) {
        columns <- primary_columns |>
            filter(origin == origin_table) |>
            pull(variable, header)
        parts[[origin_table]] <- origin_tables[[origin_table]] |>
            select(id, all_of(names(columns))) |>
            rename_with(partial(extract, columns), !id)
    }

    # Join parts.
    df <- parts |>
        reduce(full_join, by = 'id')

    # Generate the 'annotation' attribute.
    annot_table <- annot_table |>
        filter(table == table_name) |>
        select(!table) |>
        column_to_rownames('variable')

    df |>
        set_attr('annotation', annot_table) %>%
        set_class(c('annot_df', class(.)))
}


relabel_factors <- function(data) {

    relabel_columns <- annotation(data) |>
        filter(!is.na(levels))

    for (column in rownames(relabel_columns) |> intersect(colnames(data))) {

        levels <- relabel_columns[column, 'valid'] |> str_split_1(';')
        labels <- relabel_columns[column, 'levels'] |> str_split_1(';')
        data[[column]] <- data[[column]] |> factor(levels, labels)
    }

    data
}


cast_types <- function(data, casting = NULL, na_values = NULL) {

    cast_func <- default_casting
    if (!is.null(casting))
        cast_func[names(casting)] <- casting

    cast_columns <- annotation(data) |>
        filter(origin != 'ALL', origin != 'DERIVED') %>%
        { pull(., type) |> set_names(rownames(.)) }

    for (name in names(cast_columns)) {
        x <- data[[name]]
        type <- cast_columns[name]

        # Turn specified values into NAs.
        if (!is.null(na_values) && is.character(x)) {
            x[x %in% na_values] <- NA
        }

        casted_x <- cast_func[[type]](x)
        data[[name]] <- casted_x

        # Show a helpful message.
        na_before <- is.na(x)
        na_after <- is.na(casted_x)
        if (!identical(na_before, na_after)) {
            bad_values <- unique(x[na_before != na_after])
            elements <- str_c(bad_values, sep='", "')
            msg <- str_glue('Cannot coerce values of variable {name} to {type}: "{elements}"')
            warning(msg)
        } 
    }

    data
}


generate_derived <- function(data) {

    derived_columns <- annotation(data) |>
        filter(origin == 'DERIVED') %>%
        { pull(., transform) |> set_names(rownames(.)) }

    for (name in names(derived_columns))
        data[[name]] <- data |>
            with(derived_columns[name] |> parse_expr() |> eval())
    data
}


check_types_values <- function(data) {

    validators <- annotation(data) |>
        filter(!is.na(valid))
}

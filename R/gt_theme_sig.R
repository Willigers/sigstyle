#' Apply the Significance house style to a gt table
#'
#' Applies the Significance house style to a [gt::gt()] table.
#' The function provides a consistent layout for presentations and reports,
#' including corporate fonts, colours, alternating row shading, table borders,
#' header formatting, and optional summary row styling.
#'
#' Three predefined table formats are available:
#'
#' * `"presentation_half"` – standard width for tables occupying
#'   approximately half a presentation slide.
#' * `"presentation_full"` – standard width for tables spanning the full
#'   width of a presentation slide.
#' * `"report"` – standard width for use in reports and documents.
#'
#' Custom table widths can be specified through `width_cm`.
#'
#' @param gt_tbl A gt table created with [gt::gt()].
#' @param table_type Type of table layout. One of
#'   `"presentation_half"`, `"presentation_full"`, or `"report"`.
#' @param width_cm Optional table width in centimetres. If `NULL`
#'   (default), a predefined width corresponding to `table_type`
#'   is used.
#' @param invert Logical; if `TRUE`, display all titles and labels with a
#'   coloured background and white text. If `FALSE`, display coloured text on a
#'   white background.
#' @param invert_title Logical; if `TRUE`, display the title area with a
#'   coloured background and white text. If `FALSE`, display coloured text on a
#'   white background.
#' @param invert_column_labels Logical; if `TRUE`, display column labels,
#'   column spanners, and the stubhead with a coloured background and white
#'   text.
#' @param invert_stub Logical; if `TRUE`, display the stub and row group labels
#'   with a coloured background and white text.
#' @param style_summary_rows Logical; if `TRUE`, apply additional styling to
#'   summary rows and grand summary rows, including separator lines and
#'   highlighted summary labels.
#' @param body_font Font family used for table body text.
#' @param header_font Font family used for titles, column labels, row group
#'   labels, and the stub.
#' @param brand_colour Primary Significance brand colour used for text,
#'   borders, and optional header backgrounds.
#' @param stripe_colour_1 Background colour used for odd-numbered table rows.
#' @param stripe_colour_2 Background colour used for even-numbered table rows.
#'
#' @return
#' A styled gt table object.
#'
#' @details
#' The function applies the following formatting:
#'
#' * Significance house style fonts for headers and body text.
#' * Consistent border colours based on the corporate brand colour.
#' * Alternating row shading for improved readability.
#' * Custom styling for titles, subtitles, column labels, row groups, and
#'   stubs.
#' * Optional styling of summary and grand summary rows.
#' * Standard table widths for presentations and reports.
#'
#' The resulting table can be further modified using regular gt functions.
#'
#' @examples
#' library(gt)
#' library(dplyr)
#' 
#' mtcars |>
#'   tibble::rownames_to_column("vehicle") |>
#'   select(vehicle, mpg, cyl, hp) |>
#'   head(8) |>
#'   gt(rowname_col = "vehicle") |>
#'   tab_header(
#'     title = "Example table",
#'     subtitle = "Formatted using the Significance house style"
#'   ) |>
#'   gt_theme_sig(
#'     body_font = "Georgia",
#'     header_font = "Georgia"
#'   )
#'


#' # Example with inverted column labels
#' mtcars |>
#'   tibble::rownames_to_column("vehicle") |>
#'   select(vehicle, mpg, cyl, hp) |>
#'   head(8) |>
#'   gt(rowname_col = "vehicle") |>
#'   tab_header(
#'     title = "Example table",
#'     subtitle = "Formatted using the Significance house style"
#'   ) |>
#'   gt_theme_sig(
#'     body_font = "Georgia",
#'     header_font = "Georgia",
#'     invert = TRUE
#'   )
#'
#' @seealso
#' [gt::gt()], [palette_sig()], [colors_sig()]
#'
#' @export
gt_theme_sig <- function(
    gt_tbl,
    table_type = c(
      "presentation_half",
      "presentation_full",
      "report"
    ),
    width_cm = NULL,
    invert = FALSE,
    invert_title = FALSE,
    invert_column_labels = FALSE,
    invert_stub = FALSE,
    style_summary_rows = FALSE,
    body_font = c("Georgia", "serif"),
    header_font = c("Georgia", "serif"),
    brand_colour = colors_sig("blue"),
    stripe_colour_1 = colors_sig("very light blue"),
    stripe_colour_2 = colors_sig("lightest blue")
) {

  table_type <- match.arg(table_type)

  widths_cm <- c(
    presentation_half = 14.12,
    presentation_full = 30.64,
    report = 16.00
  )

  if (is.null(width_cm)) {
    width_cm <- widths_cm[[table_type]]
  }

  width_px <- round(width_cm / 2.54 * 96)

  # ---------------------------------------------------------------------------
  # Colour settings
  # ---------------------------------------------------------------------------

  # Determine colours for title
  title_text_colour <- if (invert_title | invert) "white" else brand_colour
  title_fill_colour <- if (invert_title | invert) brand_colour else "white"
  title_border_colour <- if (invert_title | invert) "white" else brand_colour

  # Determine colours for column labels
  label_text_colour <- if (invert_column_labels | invert) "white" else brand_colour
  label_fill_colour <- if (invert_column_labels | invert) brand_colour else "white"
  label_border_colour <- if (invert_column_labels | invert) "white" else brand_colour

  # Determine colours for stub
  stub_text_colour <- if (invert_stub | invert) "white" else brand_colour
  stub_fill_colour <- if (invert_stub | invert) brand_colour else "white"
  stub_border_colour <- if (invert_stub | invert) "white" else brand_colour

  # ---------------------------------------------------------------------------
  # Basic table options
  # ---------------------------------------------------------------------------

  gt_tbl <- gt_tbl |>
    gt::tab_options(
      table.background.color = "white",
      table.font.color = brand_colour,
      table.width = px(width_px),
      table.font.size = px(14),

      heading.background.color = title_fill_colour,
      heading.title.font.weight = "bold",
      heading.title.font.size = px(18),
      heading.subtitle.font.size = px(14),
      heading.border.bottom.style = "solid",
      heading.border.bottom.width = "0.5pt",
      heading.border.bottom.color = title_border_colour,

      column_labels.background.color = label_fill_colour,
      column_labels.font.size = px(14),
      column_labels.font.weight = "bold",
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = "0.5pt",
      column_labels.border.top.color = label_border_colour,
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = "0.5pt",
      column_labels.border.bottom.color = label_border_colour,

      # Remove regular horizontal body row lines
      table_body.hlines.style = "none",
      table_body.hlines.width = px(0),
      table_body.hlines.color = "transparent",

      # Keep outer body border in brand colour
      table_body.border.top.style = "solid",
      table_body.border.top.width = "0.5pt",
      table_body.border.top.color = brand_colour,
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = "0.5pt",
      table_body.border.bottom.color = brand_colour,
      
      # Set the table's top and bottom border colour
      table.border.top.style = "solid",
      table.border.top.color = label_border_colour,
      table.border.bottom.style = "solid",
      table.border.bottom.color = label_border_colour,

      # Border between stub / row labels and data body
      stub.border.style = "solid",
      stub.border.width = "0.5pt",
      stub.border.color = stub_border_colour,

      # Row group labels
      row_group.font.weight = "bold",
      row_group.border.top.style = "solid",
      row_group.border.top.width = "0.5pt",
      row_group.border.top.color = stub_border_colour,
      row_group.border.bottom.style = "solid",
      row_group.border.bottom.width = "0.5pt",
      row_group.border.bottom.color = stub_border_colour,

      data_row.padding = px(6)
    ) |>
    gt::opt_table_font(
      font = body_font
    )

  # ---------------------------------------------------------------------------
  # Alternating body row colours
  # ---------------------------------------------------------------------------

  n_rows <- nrow(gt_tbl[["_data"]])

  odd_rows <- which(seq_len(n_rows) %% 2 == 1)
  even_rows <- which(seq_len(n_rows) %% 2 == 0)

  if (length(odd_rows) > 0) {
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = cell_fill(color = stripe_colour_1),
        locations = cells_body(rows = odd_rows)
      )
  }

  if (length(even_rows) > 0) {
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = cell_fill(color = stripe_colour_2),
        locations = cells_body(rows = even_rows)
      )
  }

  # ---------------------------------------------------------------------------
  # Title and subtitle
  # ---------------------------------------------------------------------------
  gt_tbl <- gt_tbl |>
    gt::tab_style(
      style = list(
        cell_fill(color = title_fill_colour),
        cell_text(
          font = header_font,
          weight = "bold",
          color = title_text_colour
        )
      ),
      locations = cells_title(groups = "title")
    )
  
  gt_tbl <- gt_tbl |>
    gt::tab_style(
      style = list(
        cell_fill(color = title_fill_colour),
        cell_text(
          font = body_font,
          weight = "normal",
          color = title_text_colour
        )
      ),
      locations = cells_title(groups = "subtitle")
    )

  # ---------------------------------------------------------------------------
  # Column labels, spanners and stubhead
  # ---------------------------------------------------------------------------

  gt_tbl <- gt_tbl |>
    gt::tab_style(
      style = list(
        cell_fill(color = label_fill_colour),
        cell_text(
          font = header_font,
          weight = "bold",
          color = label_text_colour,
          size = px(14)
        )
      ),
      locations = cells_column_labels()
    ) |>
    gt::tab_style(
      style = list(
        cell_fill(color = label_fill_colour),
        cell_text(
          font = header_font,
          weight = "bold",
          color = label_text_colour,
          size = px(14)
        )
      ),
      locations = cells_column_spanners()
    ) |>
    gt::tab_style(
      style = list(
        cell_fill(color = label_fill_colour),
        cell_text(
          font = header_font,
          weight = "bold",
          color = label_text_colour,
          size = px(14)
        )
      ),
      locations = cells_stubhead()
    )

  # ---------------------------------------------------------------------------
  # Stub cells
  # ---------------------------------------------------------------------------

  gt_tbl <- gt_tbl |>
    gt::tab_style(
      style = list(
        cell_fill(color = stub_fill_colour),
        cell_text(
          font = header_font,
          weight = "bold",
          color = stub_text_colour,
          size = px(14)
        )
      ),
      locations = cells_stub()
    )


  # ---------------------------------------------------------------------------
  # Row group labels
  # ---------------------------------------------------------------------------

  gt_tbl <- gt_tbl |>
    gt::tab_style(
      style = list(
        cell_fill(color = stub_fill_colour),
        cell_text(
          font = header_font,
          weight = "bold",
          color = stub_text_colour,
          size = px(14)
        )
      ),
      locations = cells_row_groups()
    )

  # ---------------------------------------------------------------------------
  # Summary cells and grand summary cells
  # ---------------------------------------------------------------------------

  if (style_summary_rows) {
    gt::summary_cell_style <- list(
      cell_text(
        weight = "bold",
        color = brand_colour
      ),
      cell_borders(
        sides = "top",
        color = brand_colour,
        style = "solid",
        weight = "0.5pt"
      )
    )
    
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = summary_cell_style,
        locations = cells_summary()
      ) |>
      gt::tab_style(
        style = summary_cell_style,
        locations = cells_grand_summary()
      )
  }

  # ---------------------------------------------------------------------------
  # Summary labels in the stub
  # ---------------------------------------------------------------------------

  if (style_summary_rows) {
    summary_stub_style <- list(
      cell_fill(color = brand_colour),
      cell_text(
        weight = "bold",
        color = "white"
      ),
      cell_borders(
        sides = "top",
        color = brand_colour,
        style = "solid",
        weight = "0.5pt"
      )
    )
    
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = summary_stub_style,
        locations = cells_stub_summary()
      ) |>
      gt::tab_style(
        style = summary_stub_style,
        locations = cells_stub_grand_summary()
      )
  }

  gt_tbl
}

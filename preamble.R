library(ggplot2)

# Couleurs du thème
lkp_blue  <- grDevices::rgb(0, 34, 93, maxColorValue = 255)  # Bleu LinkPact
lkp_green <- grDevices::rgb(0, 136, 81, maxColorValue = 255) # Vert LinkPact
lkp_magenta <- grDevices::rgb(148, 0, 113, maxColorValue = 255) # Magenta LinkPact
lkp_orange <- grDevices::rgb(237, 127, 16, maxColorValue = 255) # Orange LinkPact
lkp_lightblue <- grDevices::rgb(0, 113, 148, maxColorValue = 255) # Gris LinkPact
lkp_grey <- grDevices::rgb(140, 142, 145, maxColorValue = 255) # Gris LinkPact
lkp_colors <- c(lkp_lightblue, lkp_green, lkp_magenta, lkp_orange, lkp_blue, lkp_grey)

# Thème LinkPact
theme_LinkPact <- function (base_size = 12,
                            base_family = "serif",
                            base_line_size = base_size / 16,
                            base_rect_size = base_size / 16)
{
  half_line <- base_size / 2

  theme_bw(base_size, base_family, base_line_size, base_rect_size) +
    theme(text = element_text(family = base_family, face = "plain",
                              colour = lkp_blue, size = base_size, lineheight = 0.9,
                              hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                              debug = FALSE),
          title = element_text(face = "bold"),
          axis.text = element_text(size = rel(0.8), colour = "grey30", face = "bold"),
          axis.ticks = element_line(colour = "grey20"),
          axis.title = element_text(size = rel(1.2)),
          legend.background = element_rect(colour = "grey85"),
          legend.spacing = unit(half_line, "pt"),
          legend.key.size = unit(1.2, "lines"),
          legend.text = element_text(size = rel(0.8)),
          legend.position = "right",
          legend.justification = "center",
          legend.box.spacing = unit(half_line, "pt"),
          panel.grid = element_line(colour = "grey92", linewidth = rel(0.6)),
          panel.grid.minor = element_line(linewidth = rel(0.3)),
          panel.spacing = unit(half_line, "pt"),
          panel.ontop = FALSE,
          strip.background = element_rect(fill = lkp_green, colour = "black"),
          strip.text = element_text(colour = "white", face = "bold", size = rel(0.8),
                                    margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
          strip.switch.pad.grid = unit(half_line / 2, "pt"),
          strip.switch.pad.wrap = unit(half_line / 2, "pt"),
          plot.background = element_rect(colour = "white"),
          plot.title = element_text(size = rel(1.4), hjust = 0, vjust = 1, margin = margin(b = half_line)),
          plot.title.position = "panel",
          plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(b = half_line), color = lkp_green),
          plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1, margin = margin(t = half_line), color = lkp_green),
          plot.caption.position = "panel",
          plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
          plot.tag.position = "topleft",
          plot.margin = margin(half_line, 2 * half_line, half_line, half_line))
}
theme_LinkPact() |> theme_set() # le theme est utilise pour tous les graphiques futurs

# Options de mise en page
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "",
                      rows.print = 20,
                      fig.align = "center",
                      fig.width = 10,
                      fig.asp = 0.618,
                      out.width = "100%")


# Mise en forme des tables
kable_LinkPact <- function(df, digits = 2, dims = 1, font_size = NULL, ...) {

  out <- df |> kableExtra::kbl(digits = digits,
                               format.args = list(big.mark = " ", dec = ","),
                               table.envir = "table",
                               ...)

  if (dims == 2) {

    out |>
      kableExtra::row_spec(0, bold = TRUE, color = "white", background = lkp_green, align = c("|c|", rep("c|", ncol(df)))) |>
      kableExtra::column_spec(1, border_left = TRUE, bold = TRUE, color = "white", background = lkp_green) |>
      kableExtra::column_spec(ncol(df) + 1, border_right = TRUE)

  } else {

    out |>
      kableExtra::row_spec(0, bold = TRUE, color = "white", background = lkp_green, align = c("|c|", rep("c|", ncol(df) - 1))) |>
      kableExtra::column_spec(1, border_left = TRUE) |>
      kableExtra::column_spec(ncol(df), border_right = TRUE)

  } |>
    kableExtra::kable_styling(
      latex_options = c("striped"),
      full_width = FALSE,
      font_size = font_size)
}

kable_LinkPact2 <- function(...) kable_LinkPact(..., dims = 2)

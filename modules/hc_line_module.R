# ================= HELPERS & CONSTANTS ==================

format_value <- function(values, type) {
  values <- as.double(values)
  if (isTRUE(type == "discrete")) {
    return(format(values, big.mark = ",", scientific = FALSE))
  }
  if (isTRUE(type == "continuous")) {
    return(format(round(values, 2), nsmall = 2, big.mark = ","))
  }
  if (isTRUE(type == "percentage")) {
    return(paste0(format(round(values, 2), nsmall = 2, big.mark = ","), "%"))
  }
  format(values, big.mark = ",", scientific = FALSE)
}

make_chip <- function(st, col, interactive = TRUE) {
  tags$span(
    `data-state` = if (interactive) st else NULL,
    style = paste(
      "display:inline-flex;align-items:center;gap:8px;",
      "padding:5px 12px;border-radius:6px;",
      "font-size:12px;margin:3px;font-weight:500;transition:all 0.2s;",
      if (interactive) {
        sprintf("background:#FFFFFF; border: 1.5px solid %s; color:#333; cursor:pointer;", col)
      } else {
        "background:#F5F5F5; border: 1.5px solid #DDD; color:#AAA; opacity:0.6; cursor:not-allowed;"
      }
    ),
    tags$span(style = sprintf("width:10px;height:10px;border-radius:2px;background:%s; pointer-events: none;", col)),
    st
  )
}

HC_LEGEND_JS <- "
function() {
  var legendId = '%s';
  var chartId  = '%s';

  var root = document.getElementById(legendId);
  if(!root) return;

  // Prevent attaching multiple delegated listeners if this is called multiple times
  if(root.dataset.bound === '1') return;
  root.dataset.bound = '1';

  // DOM Event Delegation: Attach one listener to the root container.
  // It will perfectly capture clicks on any chips, even after Shiny completely
  // deletes and re-renders the chunks of HTML inside it, preventing all lag.
  root.addEventListener('click', function(e) {
    var chip = e.target.closest('span[data-state]');
    if (!chip) return; // Ignore clicks if they aren't on a chip

    var chartDOM = document.getElementById(chartId);
    if (!chartDOM) return;

    // We must strictly match the id AND the exact DOM node that is currently alive.
    // Use filter and get the last chart to ignore Shiny's old memory ghosts.
    var charts = Highcharts.charts.filter(function(c) {
      return c && c.renderTo === chartDOM;
    });
    var chart = charts.length ? charts[charts.length - 1] : null;

    if (!chart || !chart.series) {
      console.log('Active chart or series array not found in DOM.');
      return;
    }

    var st = chip.getAttribute('data-state');
    var isLineVisible = false;

    // Iterate through series and toggle the clicked state
    chart.series.forEach(function(s) {
      if (s.name === st) {
        if (s.visible !== false) {
          if (typeof s.hide === 'function') s.hide();
          isLineVisible = false;
        } else {
          if (typeof s.show === 'function') s.show();
          isLineVisible = true;
        }
      }
    });

    // Visually toggle the chip styling instantly
    if (isLineVisible) {
      chip.style.opacity = '1';
      chip.style.background = '#FFFFFF';
    } else {
      chip.style.opacity = '0.4';
      chip.style.background = '#F5F5F5';
    }
  });
}
"

# Tooltip for > 10 elements (individual)
HC_TOOLTIP_INDIVIDUAL_JS <- "
function() {
  var s = '<span style=\"font-size:11px\">' + Highcharts.dateFormat('%Y', this.x) + '</span><br/>';
  s += '<span style=\"color:' + this.series.color + '\">\u25CF</span> ';
  s += '<span style=\"font-size:11px\"><b>' + this.series.name + '</b>: ' + this.point.f + '</span>';
  return s;
}
"

# Tooltip for <= 10 elements (shared, sorted)
HC_TOOLTIP_SHARED_JS <- "
function() {
  var points = this.points.slice().sort(function(a, b) { return b.y - a.y; });
  var s = '<span style=\"font-size:11px\">' + Highcharts.dateFormat('%Y', this.x) + '</span><br/>';
  points.forEach(function(p) {
    s += '<span style=\"color:' + p.series.color + '\">\u25CF</span> ';
    s += '<span style=\"font-size:11px\"><b>' + p.series.name + '</b>: ' + p.point.f + '</span><br/>';
  });
  return s;
}
"

# ================= UI HELPERS ==================

linePlotModuleUI <- function(id) {
  ns <- NS(id)
  highcharter::highchartOutput(ns("line_plot"), height = "90vh")
}

linePlotLegendUI <- function(id, width = NULL) {
  ns <- NS(id)
  tagList(
    div(
      id = paste0(ns("line_plot"), "-legend-wrapper"),
      style = "display: flex; flex-direction: column; gap: 12px; margin-bottom: 20px;",

      # Rankings Control Row
      div(
        style = "display: flex; gap: 8px; align-items: flex-end; padding: 10px; background: #F9F9F9; border-radius: 8px; border: 1px solid #EBEBEB;",
        div(
          style = "flex: 1; max-width: 150px;",
          selectInput(ns("rank_year"), "Select Year:", choices = NULL, width = "100%")
        ),
        actionButton(ns("show_ranks"), "View Ranked States", icon = icon("list-ol"), class = "btn-primary", style = "margin-bottom: 15px;")
      ),

      # Chips Container
      div(
        id = paste0(ns("line_plot"), "-legend"),
        style = paste(
          "max-width:100%;",
          "max-height:300px;",
          "overflow-y:auto;",
          "padding-right:6px;",
          "scrollbar-width:thin;",
          "-webkit-overflow-scrolling:touch;"
        ),
        tags$div(
          style = "margin:0 0 8px 0; color:#4D4D4D; font-weight:600; font-size:18px;",
          "States (Click to toggle)"
        ),
        uiOutput(ns("legend"), container = div, inline = FALSE)
      )
    )
  )
}

# =============== SERVER ==================

linePlotModuleServer <- function(id, data, dict, input_variable, input_states, Ymin, active_tab, input_color_by) {
  moduleServer(id, function(input, output, session) {
    # 1. Reactive parsing
    clean_var <- reactive({
      sub("_[12]$", "", input_variable())
    })

    var_meta <- reactive({
      cv <- clean_var()
      p_name <- dict %>%
        dplyr::filter(variable == cv) %>%
        dplyr::pull(pretty_name)
      if (length(p_name) == 0 || is.na(p_name)) p_name <- cv

      v_type <- dict %>%
        dplyr::filter(variable == cv) %>%
        dplyr::pull(type)
      if (length(v_type) == 0 || is.na(v_type)) v_type <- "continuous"

      list(pretty_name = p_name, type = v_type)
    })

    # 2. Colors map
    get_color_map <- reactive({
      req(data(), input_states(), input_color_by())
      selected_states <- input_states()
      n_states <- length(selected_states)

      if (input_color_by() == "country") {
        country_colors_base <- c("ARGENTINA" = "#74ACDF", "BRAZIL" = "#3CB371", "MEXICO" = "#E03C31")
        countries_in_df <- unique(data()$country_name)
        missing_countries <- setdiff(countries_in_df, names(country_colors_base))

        if (length(missing_countries) > 0) {
          fallback_cols <- grDevices::hcl.colors(length(missing_countries), palette = "Set 2")
          names(fallback_cols) <- missing_countries
          return(c(country_colors_base, fallback_cols))
        }
        return(country_colors_base)
      } else {
        if (n_states <= 8) {
          state_cols <- RColorBrewer::brewer.pal(max(3, n_states), "Set1")[1:n_states]
        } else if (n_states <= 12) {
          state_cols <- RColorBrewer::brewer.pal(n_states, "Paired")
        } else {
          state_cols <- grDevices::hcl.colors(n_states, palette = "Zissou 1")
        }
        return(setNames(state_cols, selected_states))
      }
    })

    # 3. Data prep for plots
    plot_data <- reactive({
      req(data(), input_variable(), input_states())

      df <- data() %>%
        dplyr::filter(state_name %in% input_states()) %>%
        dplyr::select(country_name, state_name, year, value = dplyr::all_of(input_variable())) %>%
        dplyr::distinct(country_name, state_name, year, .keep_all = TRUE) %>%
        dplyr::mutate(
          date_year = suppressWarnings(as.Date(paste0(year, "-01-01"))),
          x_dt = as.POSIXct(date_year, tz = "UTC"),
          x_ms = as.numeric(x_dt) * 1000
        ) %>%
        tidyr::drop_na(date_year)

      req(nrow(df) > 0)

      v_type <- var_meta()$type
      df %>% dplyr::mutate(.formatted_value = format_value(value, v_type))
    })

    # 4. Update Year Selector based on Data
    observe({
      req(plot_data())
      years_avail <- sort(unique(as.numeric(format(plot_data()$date_year, "%Y"))), decreasing = TRUE)

      # Retain previous selection if possible
      curr_sel <- isolate(input$rank_year)
      if (is.null(curr_sel) || !(curr_sel %in% years_avail)) {
        curr_sel <- years_avail[1]
      }

      updateSelectInput(session, "rank_year", choices = years_avail, selected = curr_sel)
    })

    # 5. Render Plot
    output$line_plot <- highcharter::renderHighchart({
      req(plot_data(), active_tab() == "graph_tab", input_color_by())

      df <- plot_data()
      color_map <- get_color_map()
      meta <- var_meta()

      n_states <- length(input_states())

      y_min <- Ymin()
      y_max_pad <- as.numeric(max(df$value, na.rm = TRUE)) * 1.05
      x_min <- min(df$x_ms, na.rm = TRUE)
      x_max <- max(df$x_ms, na.rm = TRUE)

      series_list <- purrr::map(input_states(), function(st) {
        df_state <- df %>% dplyr::filter(state_name == st)
        if (nrow(df_state) == 0) {
          return(NULL)
        }

        color_key <- if (input_color_by() == "country") unique(df_state$country_name)[1] else st
        line_color <- color_map[[color_key]] %||% "#666666"

        list(
          id = st,
          name = st,
          type = "line",
          data = purrr::transpose(list(x = df_state$x_ms, y = df_state$value, f = df_state$.formatted_value)),
          color = line_color,
          lineWidth = 2,
          marker = list(enabled = TRUE, radius = 4),
          states = list(hover = list(lineWidthPlus = 1))
        )
      }) %>% purrr::compact()

      y_label <- switch(meta$type,
        percentage = list(format = "{value:.2f}%"),
        continuous = list(formatter = highcharter::JS("function(){ return Highcharts.numberFormat(this.value, 2); }")),
        list(formatter = highcharter::JS("function(){ return Highcharts.numberFormat(this.value, 0); }"))
      )

      hc <- highcharter::highchart() %>%
        highcharter::hc_chart(
          type = "line",
          animation = FALSE,
          events = list(load = highcharter::JS(sprintf(HC_LEGEND_JS, paste0(session$ns("line_plot"), "-legend"), session$ns("line_plot"))))
        ) %>%
        highcharter::hc_xAxis(type = "datetime", min = x_min, max = x_max, gridLineWidth = 1) %>%
        highcharter::hc_yAxis(title = list(text = meta$pretty_name), min = y_min, max = if (!is.null(y_min)) y_max_pad else NULL, labels = y_label) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_add_series_list(series_list) %>%
        highcharter::hc_exporting(enabled = TRUE)

      if (n_states > 10) {
        # Individual Tooltips & Zero Lag settings
        hc <- hc %>%
          highcharter::hc_tooltip(
            shared = FALSE,
            useHTML = TRUE,
            formatter = highcharter::JS(HC_TOOLTIP_INDIVIDUAL_JS)
          ) %>%
          highcharter::hc_plotOptions(
            series = list(
              stickyTracking = FALSE, # Kills distance calculation lag
              enableMouseTracking = TRUE,
              states = list(
                hover = list(halo = FALSE), # Disable glow
                inactive = list(opacity = 1) # Prevents other lines fading out
              )
            )
          )
      } else {
        # Shared Tooltips (Ranked/Sorted) - keep all default animations
        hc <- hc %>%
          highcharter::hc_tooltip(
            shared = TRUE,
            useHTML = TRUE,
            formatter = highcharter::JS(HC_TOOLTIP_SHARED_JS)
          )
      }

      return(hc)
    })

    # 6. Handle Click to Show Modal
    observeEvent(input$show_ranks, {
      req(plot_data(), input_variable(), input$rank_year)
      target_year <- as.numeric(input$rank_year)

      df_year <- plot_data() %>%
        dplyr::filter(as.numeric(format(date_year, "%Y")) == target_year) %>%
        dplyr::arrange(desc(value))

      req(nrow(df_year) > 0)

      meta <- var_meta()
      color_map <- get_color_map()

      table_rows <- purrr::imap_chr(df_year$state_name, function(st, idx) {
        val <- df_year$.formatted_value[idx]
        col_key <- if (input_color_by() == "country") df_year$country_name[idx] else st
        col <- color_map[[col_key]] %||% "#666"

        sprintf(
          "<tr style='border-bottom: 1px solid #E6E6E6;'>
            <td style='padding: 8px 12px; font-weight: 500;'>
               <span style='display:inline-block; width:10px; height:10px; border-radius:3px; background:%s; margin-right:8px; margin-bottom:-1px;'></span>
               %s
            </td>
            <td style='padding: 8px 12px; text-align: right; font-family: monospace;'>%s</td>
          </tr>", col, st, val
        )
      })

      table_html <- sprintf(
        "<div style='max-height: 400px; overflow-y: auto; text-align: left;'>
          <table style='width: 100%%; border-collapse: collapse; font-size: 14px;'>
            <thead style='position: sticky; top: 0; background: #FFF; z-index: 1;'>
              <tr style='background-color: #F9F9F9; border-bottom: 2px solid #DDD;'>
                <th style='padding: 10px 12px; text-align: left; color: #555; position: sticky; top: 0; background: #F9F9F9;'>State</th>
                <th style='padding: 10px 12px; text-align: right; color: #555; position: sticky; top: 0; background: #F9F9F9;'>%s</th>
              </tr>
            </thead>
            <tbody>
              %s
            </tbody>
          </table>
        </div>", meta$pretty_name, paste(table_rows, collapse = "\n")
      )

      showModal(modalDialog(
        title = sprintf("Ranked States for %s", target_year),
        HTML(table_html),
        size = "m",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    # 7. Render Legend
    output$legend <- renderUI({
      req(data(), input_states(), input_variable(), input_color_by())

      # Force legend to reset when Y-axis toggle changes (since plot completely re-renders)
      force(Ymin())

      color_map <- get_color_map()
      selected_states <- input_states()

      df_presence <- data() %>%
        dplyr::filter(state_name %in% selected_states) %>%
        dplyr::group_by(state_name, country_name) %>%
        dplyr::summarise(has_data = any(!is.na(!!sym(input_variable()))), .groups = "drop")

      states_with_data <- df_presence %>%
        dplyr::filter(has_data) %>%
        dplyr::pull(state_name)
      states_without_data <- setdiff(selected_states, states_with_data)

      if (input_color_by() == "country") {
        country_order <- unique(df_presence$country_name[df_presence$has_data])
        main_ui <- lapply(country_order, function(ctry) {
          col <- color_map[[ctry]] %||% "#666666"
          states_in_ctry <- df_presence$state_name[df_presence$country_name == ctry & df_presence$has_data]

          htmltools::div(
            style = "margin-bottom:12px;",
            htmltools::span(style = "display:block;font-weight:700;font-size:12px;color:#4D4D4D;margin-bottom:4px;", ctry),
            htmltools::div(
              style = "display:flex;flex-wrap:wrap;gap:4px;",
              lapply(states_in_ctry, function(st) make_chip(st, col))
            )
          )
        })
      } else {
        main_ui <- htmltools::div(
          style = "display:flex;flex-wrap:wrap;gap:4px;",
          lapply(states_with_data, function(st) make_chip(st, color_map[[st]]))
        )
      }

      no_data_ui <- if (length(states_without_data) > 0) {
        htmltools::div(
          style = "margin-top:10px;padding-top:8px;border-top:1px solid #EEE;",
          htmltools::span(style = "display:block;font-weight:700;font-size:11px;color:#888;margin-bottom:4px;", "No Data Available"),
          htmltools::div(
            style = "display:flex;flex-wrap:wrap;gap:4px;",
            lapply(states_without_data, function(st) make_chip(st, "#CCCCCC", FALSE))
          )
        )
      }

      htmltools::tagList(main_ui, no_data_ui)
    })
  })
}

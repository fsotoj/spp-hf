server <- function(input, output, session) {
  # ==== 0.1) END POINT =======================================
  send_ga_event <- function(client_id, event_name, params = list()) {
    secret <- Sys.getenv("ZKNkvKGbTV6504car3fmFw")
    mid <- Sys.getenv("G-2D6B3PWVGG")

    url <- paste0(
      "https://www.google-analytics.com/mp/collect",
      "?measurement_id=", mid,
      "&api_secret=", secret
    )

    body <- list(
      client_id = client_id,
      events = list(
        list(name = event_name, params = params)
      )
    )

    request(url) %>%
      req_body_json(body) %>%
      req_perform()
  }


  pr <- plumber::pr()

  pr$handle("POST", "/ga", function(req, res) {
    body <- jsonlite::fromJSON(req$postBody)
    send_ga_event(body)
    return("ok")
  })

  session$registerDataObj("ga_proxy", pr, filter = "plumber")

  # print(session$getTestEndpointUrl("ga_proxy"))


  # ==== 0) BLOCKER FOR MAP- CAMERA SYNCRO =======================================

  is_navigating <- reactiveVal(FALSE)

  # ==== 0) CONSTANTS / INITIALIZATION =======================================
  current_tab <- reactiveVal("map_tab")

  map_saved_year <- reactiveVal("2005")

  observeEvent(input$tabs, {
    current_tab(input$tabs)
  })


  # Initialize Fancytree with default selection
  observeEvent(session, {
    fancytree_states_json <- get_fancytree_data_states(data)

    session$sendCustomMessage(
      "fancytree_states_data",
      list(
        # IMPORTANT: keep nested lists, NOT data.frame
        data = jsonlite::fromJSON(fancytree_states_json, simplifyVector = FALSE),
        default_selected = c(
          "ARGENTINA-CAPITAL FEDERAL",
          "BRAZIL-DISTRITO FEDERAL",
          "MEXICO-CDMX"
        )
      )
    )
  })


  ## fancy tree map

  observeEvent(session, {
    fancytree_json_vars <- get_fancytree_data_vars(
      dict %>%
        filter(viewable_map == 1, variable != "chamber_sub_leg")
    )

    session$sendCustomMessage(
      "fancytree_vars_data",
      list(
        data = jsonlite::fromJSON(fancytree_json_vars, simplifyVector = FALSE),
        default_selected = list("Executive Elections-Valid Votes")
      )
    )
  })


  ## fancy tree graph
  observeEvent(session, {
    fancytree_json_vars_graph <- get_fancytree_data_vars(
      dict %>%
        filter(viewable_graph == 1, variable != "chamber_sub_leg"),
      FALSE
    )

    session$sendCustomMessage(
      "fancytree_vars_data_graph",
      list(
        data = jsonlite::fromJSON(fancytree_json_vars_graph, simplifyVector = FALSE),
        default_selected = list("Executive Elections-Valid Votes")
      )
    )
  })


  #

  # ==== 1) GLOBAL REACTIVES ==================================================

  observeEvent(input$year_sel, {
    req(input$year_sel) # Ensure it's not NULL
    map_saved_year(as.character(input$year_sel))
  })

  # -- 1.0) states JSTree graph ----------------------

  selected_states_vector <- reactive({
    x <- input$selected_nodes_states
    if (is.null(x) || x == "" || x == "[]") {
      return(character(0))
    }

    # Fancytree sends: "COUNTRY-STATE"
    ids <- jsonlite::fromJSON(x)

    # Ensure vector
    if (!length(ids)) {
      return(character(0))
    }

    # Only state nodes contain "-" (countries do not)
    state_ids <- ids[grepl("-", ids)]

    # Extract the state part after "-"
    sapply(strsplit(state_ids, "-", fixed = TRUE), function(x) x[2])
  })


  selected_vars_vector <- reactive({
    x <- input$selected_nodes_vars2
    if (is.null(x) || x == "") {
      return(NULL)
    }

    # Fancytree sends a simple string (not a JSON array), so no fromJSON
    key <- x
    parts <- strsplit(key, "-", fixed = TRUE)[[1]]

    # Check SLED structure
    if (identical(parts[1], "Legislative Elections") &&
      length(parts) >= 3 &&
      parts[2] %in% c("Lower Chamber", "Upper Chamber")) {
      chamber <- parts[2]
      n_chamber <- ifelse(chamber == "Lower Chamber", 1, 2)

      pretty <- paste(parts[3:length(parts)], collapse = "-")

      dict %>%
        filter(pretty_name == pretty) %>%
        pull(variable) %>%
        paste0("_", n_chamber)
    } else {
      # Generic format: DATASET-Pretty Name
      pretty <- paste(parts[2:length(parts)], collapse = "-")

      dict %>%
        filter(pretty_name == pretty) %>%
        pull(variable)
    }
  })


  selected_vars_vector_graph <- reactive({
    key <- input$selected_nodes_vars_graph2
    if (is.null(key) || key == "") {
      return(NULL)
    }

    parts <- strsplit(key, "-", fixed = TRUE)[[1]]


    if (identical(parts[1], "Legislative Elections") &&
      length(parts) >= 3 &&
      parts[2] %in% c("Lower Chamber", "Upper Chamber")) {
      n_chamber <- dplyr::case_when(
        parts[2] == "Lower Chamber" ~ 1,
        parts[2] == "Upper Chamber" ~ 2
      )

      pretty <- paste(parts[3:length(parts)], collapse = "-")

      base_var <- dict %>%
        dplyr::filter(pretty_name == pretty) %>%
        dplyr::pull(variable)

      return(paste0(base_var, "_", n_chamber))
    }

    pretty <- paste(parts[2:length(parts)], collapse = "-")

    dict %>%
      dplyr::filter(pretty_name == pretty) %>%
      dplyr::pull(variable)
  })


  # -- 1.0) DATA MAP ---------------------
  data_map <- reactive({
    req(input$country_sel, input$year_sel)
    geom_filtered <- geom %>% dplyr::filter(country_name == input$country_sel)
    data_filtered <- data %>% dplyr::filter(country_name == input$country_sel, year == input$year_sel)
    dplyr::left_join(geom_filtered, data_filtered, by = "country_state_code")
  })


  # ==== 1.3) CAMERA TAB SELECTORS (SLED-driven) ==============================
  # UI renderers (camera-only)
  output$country_selector_camera <- renderUI({
    # Get all available countries
    choices <- sort(unique(SLED$country_name))

    # 1. Check if we are Navigating (Priority 1)
    if (isTRUE(is_navigating()) && !is.null(input$switch_to_camera$country)) {
      sel <- input$switch_to_camera$country
    } else {
      # 2. If NOT navigating, try to keep the CURRENT selection (Priority 2)
      # We use isolate() to peek at the value without triggering a re-render loop
      current_val <- isolate(input$country_sel_camera)

      if (!is.null(current_val) && current_val %in% choices) {
        sel <- current_val
      } else {
        # 3. Fallback to default (Priority 3)
        sel <- "BRAZIL"
      }
    }

    selectInput(
      "country_sel_camera", "Country",
      choices = choices,
      selected = sel
    )
  })

  output$state_selector_camera <- renderUI({
    req(input$country_sel_camera)

    # Get choices for the current country
    choices_vals <- SLED |>
      dplyr::filter(country_name == input$country_sel_camera) |>
      dplyr::pull(state_name) |>
      unique() |>
      sort()

    # Create labels
    choices_labs <- stringr::str_to_title(choices_vals)
    names(choices_vals) <- choices_labs

    # LOGIC TO DETERMINE SELECTION
    if (isTRUE(is_navigating()) && !is.null(input$switch_to_camera$state)) {
      # Case A: Navigating -> Use the target state
      sel <- input$switch_to_camera$state
    } else {
      # Case B: Standard interaction
      # First, check if there is ALREADY a valid selection in the box
      current_val <- isolate(input$state_sel_camera)

      if (!is.null(current_val) && current_val %in% choices_vals) {
        # Keep the current state (this preserves the state set during navigation)
        sel <- current_val
      } else {
        # Fallback to default (18th state) only if nothing valid is selected
        sel <- if (length(choices_vals)) {
          choices_vals[[min(18, length(choices_vals))]]
        } else {
          NULL
        }
      }
    }

    selectInput(
      inputId  = "state_sel_camera",
      label    = "State",
      choices  = choices_vals,
      selected = sel
    )
  })


  # Year scoping helpers (camera)
  sled_years_scoped_camera <- reactive({
    df <- SLED

    # country filter
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }

    # state filter
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }

    # chamber filter  ← THIS WAS MISSING
    # if (!is.null(input$chamber_sel_camera) && nzchar(input$chamber_sel_camera)) {
    #   df <- df[df$chamber_election_sub_leg == input$chamber_sel_camera, , drop = FALSE]
    # }


    sort(unique(df$year))
  })

  target_camera_year <- reactiveVal(NULL)
  saved_camera_year <- reactiveVal(NULL) # persists across slider recreation

  # Keep saved_camera_year in sync with the actual slider
  observeEvent(input$year_sel_camera,
    {
      saved_camera_year(input$year_sel_camera)
    },
    ignoreInit = TRUE
  )

  output$year_selector_camera_ui <- renderUI({
    req(current_tab() == "camera")

    # 1. Get available years (This SHOULD stay reactive)
    yrs <- sled_years_scoped_camera()
    req(length(yrs) > 0)

    yrs_num <- sort(as.integer(yrs))
    yrs_chr <- as.character(yrs_num)

    # 2. Determine selection
    # --- LOGIC SELECTION ---
    nav_target <- isolate(target_camera_year())
    if (!is.null(nav_target)) {
      # CASE A: Navigation target available -> Force the target year
      target <- nav_target
      if (target %in% yrs_chr) {
        sel <- target
      } else {
        # Nearest Lower Logic
        t_int <- as.integer(target)
        lower <- yrs_num[yrs_num <= t_int]
        sel <- if (length(lower) > 0) as.character(max(lower)) else tail(yrs_chr, 1)
      }
    } else {
      # CASE B: Standard interaction (no navigation)
      # Use saved_camera_year which survives slider recreation
      current_val <- isolate(saved_camera_year())
      if (!is.null(current_val) && current_val %in% yrs_chr) {
        sel <- current_val
      } else if (!is.null(current_val)) {
        # Nearest available year to what the user had selected
        t_int <- as.integer(current_val)
        lower <- yrs_num[yrs_num <= t_int]
        sel <- if (length(lower) > 0) as.character(max(lower)) else yrs_chr[1]
      } else {
        sel <- tail(yrs_chr, 1) # Default to last year
      }
    }

    # 3. Build the Slider
    shinyWidgets::sliderTextInput(
      inputId  = "year_sel_camera",
      label    = "Year",
      choices  = yrs_chr,
      selected = sel,
      grid     = TRUE,
      width    = "100%",
      animate  = shiny::animationOptions(interval = 1500, loop = FALSE)
    )
  })


  # Keep chamber selector in sync
  observeEvent(
    list(current_tab(), input$country_sel_camera, input$state_sel_camera),
    {
      req(current_tab() == "camera")

      # 1. Setup
      if (is.null(input$chamber_sel_camera)) {
        return()
      }
      ch <- available_chambers_camera()

      # If no chambers available (data gap), disable and exit
      if (!length(ch)) {
        shinyjs::disable("chamber_sel_camera")
        updateSelectInput(session, "chamber_sel_camera",
          choices = setNames(numeric(0), character(0)),
          selected = character(0)
        )
        return(invisible(NULL))
      }

      shinyjs::enable("chamber_sel_camera")
      choices_named <- .label_chambers(ch)

      # 2. Determine Selection
      # Get the current value without creating a dependency
      old_sel <- suppressWarnings(as.integer(isolate(input$chamber_sel_camera)))

      # LOGIC:
      # If we are navigating and a specific chamber was requested, use it (Optional feature)
      # Otherwise, try to keep 'old_sel'.
      # If 'old_sel' is not valid for the new state, default to the first available (ch[1]).

      target_chamber <- if (isTRUE(is_navigating()) && !is.null(input$switch_to_camera$chamber)) {
        as.integer(input$switch_to_camera$chamber)
      } else {
        NULL
      }

      if (!is.null(target_chamber) && target_chamber %in% ch) {
        new_sel <- target_chamber
      } else if (length(old_sel) && !is.na(old_sel) && old_sel %in% ch) {
        new_sel <- old_sel
      } else {
        new_sel <- ch[1]
      }

      # 3. Update
      updateSelectInput(session, "chamber_sel_camera",
        choices = choices_named,
        selected = new_sel
      )
    },
    ignoreInit = FALSE
  )


  # output$chamber_selector_camera <- renderUI({
  #   selectInput(
  #     "chamber_sel_camera", "Chamber",
  #     choices = c("Lower chamber" = 1, "Upper chamber" = 2),
  #     selected = 1
  #   )
  # })


  # Available chambers (scoped)
  available_chambers_camera <- reactive({
    df <- SLED
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }

    # --- CHANGE START: Commented out the Year filter ---
    # We want to know if a chamber exists *at all* for this state,
    # regardless of the specific year selected.

    # if (!is.null(input$year_sel_camera) && nzchar(input$year_sel_camera)) {
    #   df <- df[df$year == as.integer(input$year_sel_camera), , drop = FALSE]
    # }
    # --- CHANGE END ---

    ch <- sort(unique(suppressWarnings(as.integer(df$chamber_election_sub_leg))))
    ch <- ch[!is.na(ch) & ch %in% c(1L, 2L)]
    ch
  })

  .label_chambers <- function(v) {
    labs <- ifelse(v == 1L, "Lower chamber", ifelse(v == 2L, "Upper chamber", as.character(v)))
    stats::setNames(v, labs)
  }

  # Keep chamber selector in sync
  # Keep chamber selector in sync
  observeEvent(
    # --- CHANGE: Removed input$year_sel_camera from this list ---
    list(current_tab(), input$country_sel_camera, input$state_sel_camera),
    {
      req(current_tab() == "camera")

      # Note: We do NOT need to check input$year_sel_camera here anymore

      if (is.null(input$chamber_sel_camera)) {
        return()
      }

      ch <- available_chambers_camera()

      if (!length(ch)) {
        shinyjs::disable("chamber_sel_camera")
        updateSelectInput(session, "chamber_sel_camera",
          choices = setNames(numeric(0), character(0)),
          selected = character(0)
        )
        return(invisible(NULL))
      }

      shinyjs::enable("chamber_sel_camera")
      choices_named <- .label_chambers(ch)

      old_sel <- suppressWarnings(as.integer(isolate(input$chamber_sel_camera)))

      # Logic: If the previously selected chamber exists in the new list (e.g., Lower), keep it.
      # If not, default to the first available.
      new_sel <- if (length(old_sel) && !is.na(old_sel) && old_sel %in% ch) old_sel else ch[1]

      updateSelectInput(session, "chamber_sel_camera",
        choices = choices_named,
        selected = new_sel
      )
    },
    ignoreInit = FALSE
  )


  # ==== 2) MODALS / MESSAGES ================================================


  #  observe({
  #    tab     <- current_tab()
  #    country <- input$country_sel
  #    selvar  <- selected_vars_vector()
  #
  #    is_legislative <- FALSE
  #
  #    if (!is.null(selvar)) {
  #
  #      # remove trailing _1 or _2 from the variable
  #      selvar_clean <- sub("_[12]$", "", selvar)
  #
  #      dataset_val <- dict %>%
  #        dplyr::filter(variable == selvar_clean) %>%
  #        dplyr::pull(dataset) %>%
  #        unique()
  #
  #      is_legislative <- identical(dataset_val, "Legislative Elections")
  #
  #    }
  #
  #    if (
  #      tab == "camera" ||
  #      (tab == "map_tab" && identical(country, "MEXICO") && is_legislative)
  #    ) {
  #      showModal(
  #        tags$div(
  #          id = "devNoticeModal",
  #          modalDialog(
  #            title = HTML(""),
  #            HTML("
  #            <div style='color:#fff; font-size: 1em; text-align:center;'>
  #              <p>
  #                <strong>UNDER CONSTRUCTION</strong>
  #              </p>
  #              <p>
  #                COMING SOON!
  #              </p>
  #            </div>
  #          "),
  #            easyClose = FALSE,
  #            size = "s",
  #            footer = modalButton("Back to the tool")
  #          )
  #        )
  #      )
  #    }
  #  })

  # ==== HOW-TO MODAL ======================================================


  observeEvent(input$btn_howto, {
    # Pick the right explanation based on the current tab
    tab_name <- current_tab()

    howto_html <- switch(tab_name,
      "map_tab" = "
      <h4><i class='fa fa-map'></i> Mapping tool</h4>
      <p>
        Explore subnational data visually on an interactive map.
        Use the variable tree on the left to select an indicator,
        choose a country, and move the year slider to see how it changes over time.
      </p>
      <p>
        Hover over subnational unit for details or play the animation to view trends.
      </p>
    ",
      "graph_tab" = "
      <h4><i class='fa fa-chart-line'></i> Graphing tool</h4>
      <p>
        Create time-series plots comparing subnational indicators.
        Select one or more states from different countries and choose a variable.
        The graph updates dynamically to show trends across time.
      </p>
      <p>
        Use the legend box to toggle series visibility.
      </p>
    ",
      "camera" = "
      <h4><i class='fa fa-landmark'></i> Camera Viz tool</h4>
      <p>
        Visualize the composition of subnational legislatures.
        Select a country, state, and chamber (lower or upper chamber) to view
        party seat distributions for each election year.
      </p>

    ",
      "codebook" = "
      <h4><i class='fa fa-book-open'></i> Codebook</h4>
      <p>
        The codebook provides full definitions and sources for all variables
        included in the Subnational Politics Project datasets.
      </p>
      <p>
        Use it to understand variable meanings, coding schemes, and references.
      </p>
    ",
      "data_tab" = "
      <h4><i class='fa fa-table'></i> Databases</h4>
      <p>
        Access and download the SPP databases through the Harvard Dataverse repository.
      </p>
    ",
      "about" = "
      <h4><i class='fa fa-circle-info'></i> About</h4>
      <p>
        Learn about the Subnational Politics Project (SPP):
        its mission, team, and data infrastructure for the study of subnational politics in Latin America.
      </p>
    ",

      # default fallback
      "
      <h4><i class='fa fa-circle-question'></i> Welcome to SPP</h4>
      <p>
        Use the sidebar or the top navigation tabs to explore the different sections of the app.
      </p>
    "
    )


    showModal(
      tags$div(
        id = "howToModal",
        modalDialog(
          title = NULL,
          easyClose = TRUE,
          size = "m",
          footer = modalButton("Close"),
          HTML(howto_html)
        )
      )
    )
  })


  # “No data” message (map)
  output$no_data_message <- renderText("⚠ No data available for this country, variable and year.")


  # ==== 4) DYNAMIC UI (SELECTORS) ===========================================

  # -- 4.4) Country/Year selectors (map_tab) ---------------------------------
  output$country_selector <- renderUI({
    selectInput("country_sel", "Country",
      # choices = c("Select a country", unique(data$country_name)),
      choices = unique(data$country_name),
      selected = "MEXICO"
    )
  })


  output$year_selector <- renderUI({
    # We depend on structural changes (Country or Variable)
    req(current_tab() == "map_tab", input$country_sel, selected_vars_vector())

    # Static dataset
    df <- data

    # Core column names
    country_col <- "country_name"
    year_col <- "year"

    # Selected variable
    var_name <- selected_vars_vector()
    if (length(var_name) > 1) var_name <- var_name[[1]]
    req(is.character(var_name), var_name %in% names(df))

    # --- Filter early to minimize data in memory ---
    df_filtered <- df |>
      dplyr::filter(.data[[country_col]] == input$country_sel) |>
      dplyr::select(dplyr::all_of(c(country_col, year_col, var_name)))

    # Determine the first non-NA year
    y_min <- if (nrow(df_filtered) == 0) {
      1983L
    } else {
      valid_years <- df_filtered |>
        dplyr::filter(!is.na(.data[[var_name]])) |>
        dplyr::pull(.data[[year_col]])

      if (length(valid_years) > 0) min(valid_years, na.rm = TRUE) else min(df_filtered[[year_col]], na.rm = TRUE)
    }

    # Determine the latest available year
    y_max <- if (nrow(df_filtered) > 0) max(df_filtered[[year_col]], na.rm = TRUE) else max(df[[year_col]], na.rm = TRUE)

    # Safety fallback
    if (!is.finite(y_min) || !is.finite(y_max) || y_min > y_max) {
      y_min <- 1983L
      y_max <- 2024L
    }

    # --- NEW SELECTION LOGIC ---
    choices_vec <- as.character(seq(y_min, y_max, by = 1))

    # 1. Retrieve value from memory WITH ISOLATE
    # This breaks the re-rendering loop.
    target <- isolate(map_saved_year())

    # 2. Check if the saved year exists in the new Country's range
    final_selected <- if (!is.null(target) && target %in% choices_vec) {
      target
    } else {
      # Fallback: Try 2005, otherwise use the latest available year
      if ("2005" %in% choices_vec) "2005" else as.character(y_max)
    }

    shinyWidgets::sliderTextInput(
      inputId  = "year_sel",
      label    = "Year",
      choices  = choices_vec,
      grid     = TRUE,
      width    = "90%",
      animate  = shiny::animationOptions(interval = 1500, loop = FALSE), # Added proper animation options
      selected = final_selected
    )
  })

  # ==== 5) SHOW / HIDE CONTROLS BY TAB ======================================
  # “No data” visibility (map)
  observe({
    req(current_tab() == "map_tab", data_map())
    if (nrow(data_map()) == 0 || all(is.na(data_map()[[selected_vars_vector()]]))) {
      shinyjs::show("no_data_message")
    } else {
      shinyjs::hide("no_data_message")
    }
  })

  # Batch toggle helpers
  .combine_selector <- function(ids) if (length(ids)) paste0("#", ids, collapse = ", ") else NULL
  .batch_show <- function(ids) {
    sel <- .combine_selector(ids)
    if (!is.null(sel)) shinyjs::show(selector = sel)
  }
  .batch_hide <- function(ids) {
    sel <- .combine_selector(ids)
    if (!is.null(sel)) shinyjs::hide(selector = sel)
  }

  # Everything we may toggle anywhere in the app
  ALL_TOGGLES <- c(
    # map/graph controls
    "country_selector", "var_sel", "var_description_map", "var_description_graph", "jstree_container",
    "state_selector", # "jstree_vars_container",
    "jstree_vars_container_graph", "fancytree_vars_demo_container", "fancytree_vars_container_graph",
    "fancytree_states_container",
    # data-tab selectors
    # "country_sel2","state_sel2","db_selector","years",
    # legacy camera selector
    "camera_selector",
    # camera-tab selectors (SLED driven)
    "country_selector_camera", "state_selector_camera",
    # "chamber_selector_camera", "year_selector_camera"
    "chamber_sel_camera", "year_sel_camera"
  )

  # Given a tab, return vector of IDs to show
  .ids_to_show_for_tab <- function(tab) {
    switch(tab,
      "map_tab" = c("country_selector", "var_sel", "var_description_map", "fancytree_vars_demo_container"),
      "graph_tab" = c("var_description_graph", "state_selector", "fancytree_states_container", "fancytree_vars_container_graph"),
      # "data_tab"  = c("country_sel2","state_sel2","db_selector","years"),
      "camera" = c(
        "country_selector_camera", "state_selector_camera",
        # "chamber_selector_camera","year_selector_camera"
        "chamber_sel_camera", "year_sel_camera"
      ),
      character(0)
    )
  }

  # Main visibility controller
  observeEvent(current_tab(),
    {
      to_show <- .ids_to_show_for_tab(current_tab())
      to_hide <- setdiff(ALL_TOGGLES, to_show)
      .batch_hide(to_hide)
      .batch_show(to_show)
    },
    ignoreInit = FALSE
  )


  # ==== 6.1) FILTERED DATA FOR CAMERA =======================================
  sled_cam_filtered <- reactive({
    df <- SLED
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$chamber_sel_camera)) {
      df <- df[df$chamber_election_sub_leg == as.integer(input$chamber_sel_camera), , drop = FALSE]
    }
    if (!is.null(input$year_sel_camera) && nzchar(input$year_sel_camera)) {
      df <- df[df$year == as.integer(input$year_sel_camera), , drop = FALSE]
    }
    df
  })


  # -- 6.2) Camera info text (camera tab) ------------------------------------
  output$text_camera <- renderUI({
    req(current_tab() == "camera", sled_cam_filtered(), input$state_sel_camera)
    df <- sled_cam_filtered()

    # helpers --------------------------------------------------------
    first_or_summary <- function(x) {
      vals <- unique(na.omit(x))
      if (length(vals) == 0) {
        return("—")
      }
      if (length(vals) == 1) {
        return(as.character(vals))
      }
      paste0("varies (", paste(sort(vals), collapse = ", "), ")")
    }

    map_renewal <- function(x) {
      labs <- c(
        `1` = "Staggered every 2 years",
        `2` = "Full renewal"
      )
      u <- unique(na.omit(as.integer(x)))
      if (!length(u)) {
        return("—")
      }
      out <- ifelse(as.character(u) %in% names(labs), labs[as.character(u)], as.character(u))
      if (length(out) == 1) out else paste0("varies (", paste(out, collapse = ", "), ")")
    }

    map_system <- function(x) {
      labs <- c(
        `1` = "Proportional Representation",
        `2` = "Simple Majority",
        `3` = "Mixed (PR + Simple Majority)",
        `4` = "Mixed (PR with predefined districts)"
      )
      u <- unique(na.omit(as.integer(x)))
      if (!length(u)) {
        return("—")
      }
      out <- ifelse(as.character(u) %in% names(labs), labs[as.character(u)], as.character(u))
      if (length(out) == 1) out else paste0("varies (", paste(out, collapse = ", "), ")")
    }

    # compute --------------------------------------------------------
    total_chamber <- first_or_summary(df$total_chamber_seats_sub_leg)
    seats_contest <- first_or_summary(df$total_seats_in_contest_sub_leg)
    renewal_type <- map_renewal(df$renewal_type_sub_leg)
    elec_system <- map_system(df$electoral_system_sub_leg)
    n_parties_cont <- first_or_summary(df$num_parties_election_contest_sub_leg)

    enp_vals <- sort(unique(na.omit(as.numeric(df$enp_sub_leg))))
    enp_txt <- if (!length(enp_vals)) {
      "—"
    } else if (length(enp_vals) == 1) {
      sprintf("%.2f", enp_vals)
    } else {
      sprintf("varies (%.2f–%.2f)", min(enp_vals), max(enp_vals))
    }

    # assemble -------------------------------------------------------
    HTML(paste0(
      "<b>Chamber seats (total):</b> ", total_chamber, "<br/>",
      "<b>Seats in contest:</b> ", seats_contest, "<br/>",
      "<b>Renewal type:</b> ", renewal_type, "<br/>",
      "<b>Electoral system:</b> ", elec_system, "<br/>",
      "<b>Parties contesting:</b> ", n_parties_cont, "<br/>",
      "<b>ENPL:</b> ", enp_txt
    ))
  })


  # ==== 7) VARIABLE DESCRIPTIONS (map & graph) ==============================
  output$var_description_map <- renderUI({
    req(selected_vars_vector())

    # Fancytree sends a single string key
    key <- input$selected_nodes_vars2
    if (is.null(key) || key == "") {
      return(NULL)
    }

    parts <- strsplit(key, "-", fixed = TRUE)[[1]]

    # Detect chamber (only applies to Legislative Elections)
    chamber <- NULL
    if (identical(parts[1], "Legislative Elections") &&
      length(parts) >= 3 &&
      parts[2] %in% c("Lower Chamber", "Upper Chamber")) {
      chamber <- parts[2]
    }

    # Remove _1/_2 suffix from variable ID
    clean_var <- sub("_[12]$", "", selected_vars_vector())

    # Retrieve metadata
    var_info <- dict %>%
      dplyr::filter(variable == clean_var) %>%
      dplyr::slice(1)

    # Build text
    text_d <- paste0(
      "<div>You are seeing <strong>",
      var_info$description_for_ui[1], "</strong>",
      if (!is.null(chamber)) paste0(" (", chamber, ")") else "",
      "; from the Subnational <strong>", parts[1],
      "</strong> Database.", ifelse(is.na(var_info$add_indices[1]), "", var_info$add_indices[1]), "</div>"
    )

    HTML(text_d)
  })


  output$var_description_graph <- renderUI({
    req(selected_vars_vector_graph())

    # Fancytree sends a single string key
    key <- input$selected_nodes_vars_graph2
    if (is.null(key) || key == "") {
      return(NULL)
    }

    parts <- strsplit(key, "-", fixed = TRUE)[[1]]

    # Detect chamber type (only for Legislative Elections)
    chamber <- NULL
    if (identical(parts[1], "Legislative Elections") &&
      length(parts) >= 3 &&
      parts[2] %in% c("Lower Chamber", "Upper Chamber")) {
      chamber <- parts[2]
    }

    # Clean variable (remove _1 or _2 suffix)
    clean_var <- sub("_[12]$", "", selected_vars_vector_graph())

    # Metadata row
    var_info <- dict %>%
      dplyr::filter(variable == clean_var) %>%
      dplyr::slice(1)

    # Build UI string
    text_d <- paste0(
      "<div>You are seeing <strong>",
      var_info$description_for_ui[1], "</strong>",
      if (!is.null(chamber)) paste0(" (", chamber, ")") else "",
      "; from the Subnational <strong>",
      parts[1],
      "</strong> Database.</div>"
    )

    HTML(text_d)
  })


  # Keep default options of var_sel and var_sel2 in sync
  observe({
    updateSelectInput(
      session, "var_sel",
      choices = dict$pretty_name[dict$viewable_map == 1],
      selected = "Valid Votes"
    )
    updateSelectInput(
      session, "var_sel2",
      choices = dict$pretty_name[dict$viewable_graph == 1],
      selected = "Voter Turnout Percentage"
    )
  })


  # ==== 9) MODULES (map / lines / table / camera) ===========================


  mapModuleServer(
    id = "map1",
    data_map = data_map,
    input_var_sel = selected_vars_vector,
    dict = dict,
    country_bboxes = country_bboxes,
    input_country_sel = reactive(input$country_sel),
    active_tab = current_tab
  )

  linePlotModuleServer(
    id = "lp",
    data = reactive(data),
    dict = dict,
    input_variable = selected_vars_vector_graph,
    input_states = selected_states_vector,
    Ymin = reactive(if (isTRUE(input$force_y0)) 0 else NULL),
    active_tab = current_tab,
    input_color_by = reactive({
      if (input$color_by_state) "state" else "country"
    })
  )


  # Hemicycle: still using original module (with inputs), now pointing to camera-only selectors.
  camaraServer(
    id = "cam",
    data = SLED, # if you later refactor the module, change to data_r = sled_cam_filtered
    state_r = reactive(input$state_sel_camera),
    country_sel_camera = reactive(input$country_sel_camera),
    chamber_r = reactive(input$chamber_sel_camera),
    year_r = reactive(input$year_sel_camera),
    party_col = "party_name_sub_leg",
    seats_col = "total_seats_party_sub_leg",
    state_col = "state_name",
    chamber_filter_col = "chamber_election_sub_leg",
    year_col = "year"
  )


  sppAboutModuleServer("about")

  spp_mvp_server("spp1", current_tab = current_tab)


  # ==== 11) PDF VIEWER ======================================================
  output$pdf_visor <- renderUI({
    tags$iframe(
      style = "height:800px; width:100%;",
      src = "docs/SPP_codebook.pdf"
    )
  })
  # ==== 12) TABS ======================================================
  #
  # observeEvent(input$tab_about, { updateTabItems(session, "tabs", "about") })
  # observeEvent(input$tab_map, { updateTabItems(session, "tabs", "map_tab") })
  # observeEvent(input$tab_graph, { updateTabItems(session, "tabs", "graph_tab") })
  # observeEvent(input$tab_camera, { updateTabItems(session, "tabs", "camera") })
  # observeEvent(input$tab_codebook, { updateTabItems(session, "tabs", "codebook") })
  # observeEvent(input$tab_data, { updateTabItems(session, "tabs", "data_tab") })
  #
  #
  #
  #

  # ==== 13) SWIPE ======================================================
  observeEvent(input$sidebar_swipe, {
    if (input$sidebar_swipe == "left") {
      shinyjs::runjs("
      console.log('Force closing sidebar (server).');
      $('body')
        .removeClass('sidebar-open')
        .addClass('sidebar-collapse');
    ")
    }
  })

  # ==== 14) GO to camera tab ======================================================

  observeEvent(input$switch_to_camera, {
    req(input$switch_to_camera)

    # 0. Store navigation target BEFORE anything else (timing-independent)
    target_camera_year(as.character(input$switch_to_camera$year))

    # 1. LOCK & LOAD
    is_navigating(TRUE)
    shinyjs::show("global-loader")

    # 2. SWITCH TAB
    updateTabItems(session, "tabs", selected = "camera")

    # 3. UNLOCK (safety delay for loader only)
    shinyjs::delay(2000, {
      is_navigating(FALSE)
      shinyjs::hide("global-loader")
      print("LOG [Switch]: Navigation complete.")
    })
  })

  # Clear navigation target when user manually changes camera selectors
  # (but NOT during navigation, so the cascade can finish first)
  observeEvent(
    list(input$country_sel_camera, input$state_sel_camera, input$chamber_sel_camera),
    {
      if (!isTRUE(is_navigating())) {
        target_camera_year(NULL)
      }
    },
    ignoreInit = TRUE
  )


  # ==== 14) GO to graph tab ======================================================

  # ==== 14) GO to graph tab (UPDATER) =========================================
  # ==== 14) GO to graph tab (UPDATER) =========================================
  observeEvent(input$switch_to_graph, {
    req(input$switch_to_graph)

    print("LOG [Switch]: Button clicked.")

    # 1. PREPARE IDS
    # A) States (Always valid, logic remains same)
    target_country <- toupper(input$switch_to_graph$country)
    target_state <- toupper(input$switch_to_graph$state)
    state_node_id <- paste0(target_country, "-", target_state)

    # B) Variable (New Validation Logic)
    target_var <- input$switch_to_graph$variable
    var_node_id <- NULL
    variable_is_missing <- FALSE

    if (!is.null(target_var)) {
      # Remove suffix to match dictionary
      clean_var <- sub("_[12]$", "", target_var)

      # Look up metadata ONLY for variables allowed in the graph
      var_meta <- dict %>%
        dplyr::filter(variable == clean_var, viewable_graph == 1) %>%
        dplyr::slice(1)

      if (nrow(var_meta) > 0) {
        # Success: Variable exists in Graph
        ds <- var_meta$dataset
        pretty <- var_meta$pretty_name

        if (ds == "Legislative Elections") {
          chamber_str <- if (grepl("_2$", target_var)) "Upper Chamber" else "Lower Chamber"
          var_node_id <- paste(ds, chamber_str, pretty, sep = "-")
        } else {
          var_node_id <- paste(ds, pretty, sep = "-")
        }
      } else {
        # Failure: Variable is Map-only
        variable_is_missing <- TRUE
      }
    }

    # 2. HANDLE MISSING VARIABLE (Show Warning)
    if (variable_is_missing) {
      showModal(modalDialog(
        # Use tags$span to group the icon and text safely
        title = tags$span(
          # No manual color needed: it will pick up the dark color (#111) from your CSS header
          shiny::icon("exclamation-triangle", style = "margin-right: 8px;"),
          "Variable Not Available"
        ),
        HTML(paste0(
          "<div style='margin-top: 5px;'>",
          "<p><strong>", target_var, "</strong> is available in the Map tool but cannot be graphed.</p>",
          "<p>Switching to Graph view with the current state selection only.</p>",
          "</div>"
        )),
        easyClose = TRUE,
        size = "m",
        # The footer will automatically pick up your purple button style
        footer = modalButton("OK")
      ))
    }

    # 3. LOCK & LOAD
    is_navigating(TRUE)
    shinyjs::show("global-loader")

    # 4. SWITCH TAB
    updateTabItems(session, "tabs", selected = "graph_tab")

    # 5. SEND UPDATE COMMANDS
    shinyjs::delay(500, {
      # Update State Tree (Always)
      session$sendCustomMessage("update_fancytree_selection", list(id = state_node_id))

      # Update Variable Tree (Only if valid)
      if (!is.null(var_node_id)) {
        session$sendCustomMessage("update_fancytree_variable_graph", list(id = var_node_id))
      }
    })

    # 6. UNLOCK
    shinyjs::delay(1500, {
      is_navigating(FALSE)
      shinyjs::hide("global-loader")
      print("LOG [Switch]: Navigation complete.")
    })
  })
}

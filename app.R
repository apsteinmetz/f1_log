library(shiny)
library(bslib)
library(tidyverse)
library(httr2)
library(lubridate)
library(leaflet)
library(glue)

season_year <- 2026
base_url <- "https://api.jolpi.ca/ergast/f1"

country_iso_lookup <- c(
  "Abu Dhabi" = "ae",
  "United Arab Emirates" = "ae",
  "Argentina" = "ar",
  "Australia" = "au",
  "Austria" = "at",
  "Azerbaijan" = "az",
  "Bahrain" = "bh",
  "Belgium" = "be",
  "Brazil" = "br",
  "Canada" = "ca",
  "China" = "cn",
  "France" = "fr",
  "Germany" = "de",
  "Hungary" = "hu",
  "India" = "in",
  "Italy" = "it",
  "Japan" = "jp",
  "Mexico" = "mx",
  "Monaco" = "mc",
  "Netherlands" = "nl",
  "Portugal" = "pt",
  "Qatar" = "qa",
  "Russia" = "ru",
  "Saudi Arabia" = "sa",
  "Singapore" = "sg",
  "South Africa" = "za",
  "South Korea" = "kr",
  "Spain" = "es",
  "Switzerland" = "ch",
  "United Kingdom" = "gb",
  "UK" = "gb",
  "United States" = "us",
  "USA" = "us"
)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

flag_badge <- function(country, size = 24) {
  if (
    is.null(country) || length(country) == 0 || is.na(country) || country == ""
  ) {
    return(tags$span("Unknown"))
  }
  code <- country_iso_lookup[country]
  if (is.na(code) || is.null(code)) {
    return(tags$span(country))
  }
  height <- round(size * 3 / 4)
  tags$span(
    tags$img(
      src = glue("https://flagcdn.com/{size}x{height}/{tolower(code)}.png"),
      width = size,
      height = height,
      class = "me-2",
      alt = country
    ),
    country
  )
}

fetch_json <- function(path, limit = 400) {
  req <- request(glue("{base_url}/{path}.json")) |> req_url_query(limit = limit)
  tryCatch(
    req |>
      req_perform() |>
      resp_body_json(check_type = FALSE, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

get_schedule <- function(season) {
  resp <- fetch_json(glue("{season}/races"), limit = 200)
  if (is.null(resp) || is.null(resp$MRData$RaceTable$Races)) {
    return(tibble())
  }
  races <- resp$MRData$RaceTable$Races %||% list()
  if (length(races) == 0) {
    return(tibble())
  }
  map_dfr(races, function(r) {
    loc <- r$Circuit$Location %||% list()
    tibble(
      season = r$season %||% season,
      round = as.integer(r$round %||% NA_integer_),
      race = r$raceName %||% "",
      circuit = r$Circuit$circuitName %||% "",
      country = loc$country %||% "",
      locality = loc$locality %||% "",
      lat = as.numeric(loc$lat %||% NA_real_),
      long = as.numeric(loc$long %||% NA_real_),
      date = r$date %||% NA_character_,
      time = r$time %||% NA_character_
    )
  }) |>
    arrange(round)
}

get_results <- function(season, round) {
  resp <- fetch_json(glue("{season}/{round}/results"), limit = 200)
  if (is.null(resp) || is.null(resp$MRData$RaceTable$Races)) {
    return(tibble(
      position = integer(),
      position_text = character(),
      points = numeric(),
      driver = character(),
      driver_id = character(),
      constructor = character(),
      grid = integer(),
      status = character(),
      fastest_rank = character(),
      fastest_time = character(),
      fastest_speed = character()
    ))
  }
  races <- resp$MRData$RaceTable$Races %||% list()
  if (length(races) == 0 || is.null(races[[1]]$Results)) {
    return(tibble(
      position = integer(),
      position_text = character(),
      points = numeric(),
      driver = character(),
      driver_id = character(),
      constructor = character(),
      grid = integer(),
      status = character(),
      fastest_rank = character(),
      fastest_time = character(),
      fastest_speed = character()
    ))
  }
  map_dfr(races[[1]]$Results, function(x) {
    tibble(
      position = as.integer(x$position %||% NA_integer_),
      position_text = x$positionText %||% "",
      points = as.numeric(x$points %||% NA_real_),
      driver = paste(x$Driver$givenName, x$Driver$familyName),
      driver_id = x$Driver$driverId %||% "",
      constructor = x$Constructor$name %||% "",
      grid = as.integer(x$grid %||% NA_integer_),
      status = x$status %||% "",
      fastest_rank = x$FastestLap$rank %||% NA_character_,
      fastest_time = x$FastestLap$Time$time %||% NA_character_,
      fastest_speed = x$FastestLap$AverageSpeed$speed %||% NA_character_
    )
  }) |>
    arrange(position)
}

get_qualifying <- function(season, round) {
  resp <- fetch_json(glue("{season}/{round}/qualifying"), limit = 200)
  if (is.null(resp) || is.null(resp$MRData$RaceTable$Races)) {
    return(tibble())
  }
  races <- resp$MRData$RaceTable$Races %||% list()
  if (length(races) == 0 || is.null(races[[1]]$QualifyingResults)) {
    return(tibble())
  }
  map_dfr(races[[1]]$QualifyingResults, function(x) {
    tibble(
      position = as.integer(x$position %||% NA_integer_),
      driver = paste(x$Driver$givenName, x$Driver$familyName),
      constructor = x$Constructor$name %||% "",
      q1 = x$Q1 %||% "",
      q2 = x$Q2 %||% "",
      q3 = x$Q3 %||% ""
    )
  }) |>
    arrange(position)
}

parse_duration_seconds <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "") {
    return(NA_real_)
  }
  if (str_detect(x, ":")) {
    parts <- str_split(x, ":", n = 2)[[1]]
    as.numeric(parts[1]) * 60 + as.numeric(parts[2])
  } else {
    as.numeric(x)
  }
}

get_pitstops <- function(season, round) {
  resp <- fetch_json(glue("{season}/{round}/pitstops"), limit = 400)
  if (is.null(resp) || is.null(resp$MRData$RaceTable$Races)) {
    return(tibble(
      driver_id = character(),
      lap = integer(),
      stop = integer(),
      time = character(),
      duration = character(),
      duration_sec = numeric()
    ))
  }
  races <- resp$MRData$RaceTable$Races %||% list()
  if (length(races) == 0 || is.null(races[[1]]$PitStops)) {
    return(tibble(
      driver_id = character(),
      lap = integer(),
      stop = integer(),
      time = character(),
      duration = character(),
      duration_sec = numeric()
    ))
  }
  map_dfr(races[[1]]$PitStops, function(x) {
    tibble(
      driver_id = x$driverId %||% "",
      lap = as.integer(x$lap %||% NA_integer_),
      stop = as.integer(x$stop %||% NA_integer_),
      time = x$time %||% "",
      duration = x$duration %||% "",
      duration_sec = parse_duration_seconds(x$duration %||% NA_character_)
    )
  }) |>
    arrange(duration_sec)
}

get_driver_standings <- function(season) {
  resp <- fetch_json(glue("{season}/driverstandings"), limit = 200)
  if (is.null(resp) || is.null(resp$MRData$StandingsTable$StandingsLists)) {
    return(tibble())
  }
  lists <- resp$MRData$StandingsTable$StandingsLists %||% list()
  if (length(lists) == 0 || is.null(lists[[1]]$DriverStandings)) {
    return(tibble())
  }
  map_dfr(lists[[1]]$DriverStandings, function(x) {
    tibble(
      position = as.integer(x$position %||% NA_integer_),
      points = as.numeric(x$points %||% NA_real_),
      wins = as.integer(x$wins %||% NA_integer_),
      driver = paste(x$Driver$givenName, x$Driver$familyName),
      constructor = x$Constructors[[1]]$name %||% ""
    )
  }) |>
    arrange(position)
}

get_constructor_standings <- function(season) {
  resp <- fetch_json(glue("{season}/constructorstandings"), limit = 200)
  if (is.null(resp) || is.null(resp$MRData$StandingsTable$StandingsLists)) {
    return(tibble())
  }
  lists <- resp$MRData$StandingsTable$StandingsLists %||% list()
  if (length(lists) == 0 || is.null(lists[[1]]$ConstructorStandings)) {
    return(tibble())
  }
  map_dfr(lists[[1]]$ConstructorStandings, function(x) {
    tibble(
      position = as.integer(x$position %||% NA_integer_),
      points = as.numeric(x$points %||% NA_real_),
      wins = as.integer(x$wins %||% NA_integer_),
      constructor = x$Constructor$name %||% ""
    )
  }) |>
    arrange(position)
}

get_drivers <- function(season) {
  resp <- fetch_json(glue("{season}/drivers"), limit = 400)
  if (is.null(resp) || is.null(resp$MRData$DriverTable$Drivers)) {
    return(character())
  }
  drivers <- resp$MRData$DriverTable$Drivers %||% list()
  map_chr(drivers, function(x) paste(x$givenName, x$familyName))
}

get_constructors <- function(season) {
  resp <- fetch_json(glue("{season}/constructors"), limit = 200)
  if (is.null(resp) || is.null(resp$MRData$ConstructorTable$Constructors)) {
    return(character())
  }
  constructors <- resp$MRData$ConstructorTable$Constructors %||% list()
  map_chr(constructors, ~ .x$name %||% "")
}

get_all_results <- function(season) {
  resp <- fetch_json(glue("{season}/results"), limit = 1200)
  if (is.null(resp) || is.null(resp$MRData$RaceTable$Races)) {
    return(tibble())
  }
  races <- resp$MRData$RaceTable$Races %||% list()
  if (length(races) == 0) {
    return(tibble())
  }
  map_dfr(races, function(r) {
    res <- r$Results %||% list()
    if (length(res) == 0) {
      return(tibble())
    }
    map_dfr(res, function(x) {
      tibble(
        round = as.integer(r$round %||% NA_integer_),
        country = r$Circuit$Location$country %||% "",
        race = r$raceName %||% "",
        date = r$date %||% NA_character_,
        driver = paste(x$Driver$givenName, x$Driver$familyName),
        constructor = x$Constructor$name %||% "",
        points = as.numeric(x$points %||% NA_real_),
        position = as.integer(x$position %||% NA_integer_),
        grid = as.integer(x$grid %||% NA_integer_),
        status = x$status %||% ""
      )
    })
  })
}

is_past_race <- function(date_chr, time_chr) {
  if (is.na(date_chr) || date_chr == "") {
    return(FALSE)
  }
  if (!is.na(time_chr) && time_chr != "") {
    dt <- ymd_hms(paste(date_chr, time_chr), quiet = TRUE, tz = "UTC")
  } else {
    dt <- ymd(date_chr, quiet = TRUE)
  }
  if (is.na(dt)) {
    return(FALSE)
  }
  dt < now(tzone = "UTC")
}

addResourcePath("img", "img")

theme <- bs_theme(
  bootswatch = "cosmo",
  primary = "#e10600",
  base_font = font_google("Fira Sans")
)

ui <- page_navbar(
  title = tags$div(
    class = "d-flex align-items-center gap-3",
    tags$img(src = "img/f1_logo.png", height = 40, alt = "F1"),
    tags$span(class = "fw-bold", "F1 Season Tracker")
  ),
  nav_panel(
    "Races",
    uiOutput("races_view")
  ),
  nav_panel(
    "Predictions",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Season Predictions"),
        card_body(
          h5("Drivers"),
          uiOutput("driver_predictions"),
          h5(class = "mt-3", "Teams"),
          uiOutput("team_predictions"),
          h5(class = "mt-3", "Most Podiums"),
          layout_columns(
            selectInput("most_podium_driver", "Driver", choices = NULL),
            selectInput("most_podium_team", "Team", choices = NULL)
          ),
          actionButton(
            "clear_predictions",
            "Clear",
            class = "btn-secondary mt-3"
          )
        )
      ),
      card(
        card_header("Current Standings"),
        card_body(
          h5("Drivers"),
          tableOutput("driver_standings_table"),
          h5(class = "mt-3", "Constructors"),
          tableOutput("constructor_standings_table")
        )
      )
    )
  ),
  nav_panel(
    "Driver Points",
    card(
      card_body(tableOutput("driver_points"))
    )
  ),
  nav_panel(
    "Team Points",
    card(
      card_body(tableOutput("team_points"))
    )
  ),
  nav_panel(
    "About",
    card(
      card_body(
        p("Created by Art Steinmetz with Shiny for R from Posit."),
        p(
          "Data Source: ",
          tags$a(
            href = "https://github.com/jolpica/jolpica-f1/tree/main/docs",
            "jolpica-f1 API"
          )
        )
      )
    )
  ),
  nav_spacer(),
  nav_item(actionButton(
    "refresh",
    "Refresh Data",
    icon = icon("arrow-rotate-right"),
    class = "btn-primary"
  )),
  theme = theme,
  fillable = TRUE
)

server <- function(input, output, session) {
  refresh_trigger <- reactiveVal(Sys.time())

  observeEvent(input$refresh, refresh_trigger(Sys.time()))

  schedule_data <- reactive({
    refresh_trigger()
    get_schedule(season_year)
  })

  drivers_list <- reactive({
    refresh_trigger()
    get_drivers(season_year)
  })

  constructors_list <- reactive({
    refresh_trigger()
    get_constructors(season_year)
  })

  driver_standings <- reactive({
    refresh_trigger()
    get_driver_standings(season_year)
  })

  constructor_standings <- reactive({
    refresh_trigger()
    get_constructor_standings(season_year)
  })

  season_results <- reactive({
    refresh_trigger()
    get_all_results(season_year)
  })

  selected_round <- reactiveVal(NULL)

  observeEvent(schedule_data(), {
    sched <- schedule_data()
    walk(sched$round, function(rd) {
      input_id <- paste0("race_", rd)
      observeEvent(
        input[[input_id]],
        {
          selected_round(rd)
        },
        ignoreInit = TRUE
      )
    })
  })

  observeEvent(input$refresh, {
    selected_round(NULL)
  })

  race_info <- reactive({
    sched <- schedule_data()
    sel <- selected_round()
    if (is.null(sel) || nrow(sched) == 0) {
      return(tibble())
    }
    sched |> filter(round == sel) |> slice_head(n = 1)
  })

  race_past <- reactive({
    info <- race_info()
    if (nrow(info) == 0) {
      return(FALSE)
    }
    is_past_race(info$date[[1]], info$time[[1]])
  })

  qual_data <- reactive({
    if (!race_past()) {
      return(tibble())
    }
    get_qualifying(season_year, selected_round())
  })

  race_results <- reactive({
    if (!race_past()) {
      return(tibble(
        position = integer(),
        driver = character(),
        constructor = character(),
        points = numeric(),
        status = character(),
        driver_id = character(),
        grid = integer(),
        fastest_rank = character(),
        fastest_time = character(),
        fastest_speed = character()
      ))
    }
    get_results(season_year, selected_round())
  })

  pit_data <- reactive({
    if (!race_past()) {
      return(tibble(
        driver_id = character(),
        lap = integer(),
        stop = integer(),
        time = character(),
        duration = character(),
        duration_sec = numeric()
      ))
    }
    get_pitstops(season_year, selected_round())
  })

  race_highlights <- reactive({
    res <- race_results()
    pits <- pit_data()
    if (nrow(res) == 0 || ncol(res) == 0) {
      return(list(
        fastest_lap = list(label = "N/A", detail = ""),
        fastest_pit = list(label = "N/A", detail = ""),
        overtakes = list(label = "N/A", detail = ""),
        penalties = list(label = "N/A", detail = "")
      ))
    }

    fastest_row <- res |>
      filter(!is.na(fastest_rank) & fastest_rank == "1") |>
      slice_head(n = 1)
    fastest_lap <- if (nrow(fastest_row) == 0) {
      list(label = "N/A", detail = "")
    } else {
      list(
        label = fastest_row$driver,
        detail = paste0(
          fastest_row$fastest_time,
          " (",
          fastest_row$fastest_speed,
          " kph)"
        )
      )
    }

    fastest_pit_row <- pits |>
      filter(!is.na(duration_sec)) |>
      slice_min(duration_sec, n = 1)
    fastest_pit <- if (nrow(fastest_pit_row) == 0) {
      list(label = "N/A", detail = "")
    } else {
      driver_name <- res |>
        filter(driver_id == fastest_pit_row$driver_id) |>
        slice_head(n = 1) |>
        pull(driver)
      list(
        label = driver_name %||% fastest_pit_row$driver_id,
        detail = paste0(
          fastest_pit_row$duration,
          " (Lap ",
          fastest_pit_row$lap,
          ")"
        )
      )
    }

    overtakes_row <- res |>
      filter(!is.na(grid), !is.na(position)) |>
      mutate(change = grid - position) |>
      slice_max(change, n = 1, with_ties = FALSE)
    overtakes <- if (nrow(overtakes_row) == 0) {
      list(label = "N/A", detail = "")
    } else {
      list(
        label = overtakes_row$driver,
        detail = paste0("+", overtakes_row$change, " positions")
      )
    }

    penalties_row <- res |>
      filter(str_detect(str_to_lower(status), "pen")) |>
      slice_head(n = 1)
    penalties <- if (nrow(penalties_row) == 0) {
      list(label = "N/A", detail = "")
    } else {
      list(label = penalties_row$driver, detail = penalties_row$status)
    }

    list(
      fastest_lap = fastest_lap,
      fastest_pit = fastest_pit,
      overtakes = overtakes,
      penalties = penalties
    )
  })

  output$circuit_map <- renderLeaflet({
    info <- race_info()
    if (nrow(info) == 0 || is.na(info$lat) || is.na(info$long)) {
      return(NULL)
    }
    leaflet() |>
      addTiles() |>
      addMarkers(
        lng = info$long,
        lat = info$lat,
        label = info$circuit
      )
  })

  output$qual_table <- renderTable(
    {
      qual_data()
    },
    striped = TRUE,
    bordered = TRUE,
    width = "100%",
    spacing = "s",
    digits = 3
  )

  output$result_table <- renderTable(
    {
      race_results() |> select(position, driver, constructor, points, status)
    },
    striped = TRUE,
    bordered = TRUE,
    width = "100%",
    spacing = "s",
    digits = 3
  )

  observeEvent(input$close_race, {
    selected_round(NULL)
  })

  output$races_view <- renderUI({
    if (is.null(selected_round())) {
      # Show schedule grid
      sched <- schedule_data()
      if (nrow(sched) == 0) {
        return(tags$p("No schedule data available yet."))
      }
      cards <- lapply(seq_len(nrow(sched)), function(i) {
        r <- sched[i, ]
        round_val <- r$round[[1]]
        if (is.na(round_val)) {
          round_val <- i
        }
        race_date <- if (!is.na(r$date[[1]])) {
          format(ymd(r$date[[1]]), "%b %d")
        } else {
          ""
        }
        actionButton(
          paste0("race_", round_val),
          label = NULL,
          class = "p-0 border-0 bg-transparent w-100",
          card(
            class = "shadow-sm h-100",
            card_body(
              class = "text-center",
              flag_badge(r$country[[1]], size = 48),
              h5(class = "mt-2 mb-1", r$country[[1]]),
              p(class = "mb-0 text-muted", race_date)
            )
          )
        )
      })
      do.call(
        layout_column_wrap,
        c(list(width = "300px", heights_equal = "all"), cards)
      )
    } else {
      # Show race detail page
      info <- race_info()
      if (nrow(info) == 0) {
        return(tags$p("Race not found."))
      }
      metrics <- race_highlights()
      tagList(
        div(
          class = "mb-3",
          actionButton(
            "close_race",
            "← Back to Schedule",
            class = "btn-secondary"
          )
        ),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          card(
            card_header(paste("Round", info$round[[1]], "-", info$race[[1]])),
            card_body(
              p(flag_badge(info$country[[1]])),
              p(info$circuit[[1]]),
              p(info$locality[[1]]),
              p(ifelse(race_past(), "Completed", "Upcoming")),
              leafletOutput("circuit_map", height = 260)
            )
          ),
          card(
            card_header("Qualifying"),
            card_body(
              if (race_past()) tableOutput("qual_table") else p("No data yet.")
            )
          ),
          card(
            card_header("Race Results"),
            card_body(
              if (race_past()) {
                tableOutput("result_table")
              } else {
                p("No data yet.")
              }
            )
          ),
          card(
            card_header("Highlights"),
            card_body(
              value_box(
                title = "Fastest Lap",
                value = metrics$fastest_lap$label,
                p(metrics$fastest_lap$detail)
              ),
              value_box(
                title = "Fastest Pit Stop",
                value = metrics$fastest_pit$label,
                p(metrics$fastest_pit$detail)
              ),
              value_box(
                title = "Most Overtakes",
                value = metrics$overtakes$label,
                p(metrics$overtakes$detail)
              ),
              value_box(
                title = "Penalties",
                value = metrics$penalties$label,
                p(metrics$penalties$detail)
              )
            )
          )
        )
      )
    }
  })

  preds <- reactiveValues(drivers = NULL, teams = NULL)

  observeEvent(
    drivers_list(),
    {
      preds$drivers <- rep(NA_character_, length(drivers_list()))
      updateSelectInput(session, "most_podium_driver", choices = drivers_list())
    },
    ignoreInit = FALSE
  )

  observeEvent(
    constructors_list(),
    {
      preds$teams <- rep(NA_character_, length(constructors_list()))
      updateSelectInput(
        session,
        "most_podium_team",
        choices = constructors_list()
      )
    },
    ignoreInit = FALSE
  )

  available_choice <- function(all_values, current, pos) {
    taken <- current[-pos]
    available <- setdiff(all_values, taken[!is.na(taken)])
    sort(unique(c(current[[pos]], available)))
  }

  output$driver_predictions <- renderUI({
    drv <- drivers_list()
    if (length(drv) == 0) {
      return(tags$p("Drivers unavailable."))
    }
    lapply(seq_along(drv), function(i) {
      selectInput(
        inputId = paste0("driver_rank_", i),
        label = paste("P", i),
        choices = available_choice(drv, preds$drivers, i),
        selected = preds$drivers[[i]],
        width = "100%"
      )
    })
  })

  output$team_predictions <- renderUI({
    tm <- constructors_list()
    if (length(tm) == 0) {
      return(tags$p("Constructors unavailable."))
    }
    lapply(seq_along(tm), function(i) {
      selectInput(
        inputId = paste0("team_rank_", i),
        label = paste("P", i),
        choices = available_choice(tm, preds$teams, i),
        selected = preds$teams[[i]],
        width = "100%"
      )
    })
  })

  observeEvent(input$clear_predictions, {
    preds$drivers <- if (length(drivers_list()) == 0) {
      NULL
    } else {
      rep(NA_character_, length(drivers_list()))
    }
    preds$teams <- if (length(constructors_list()) == 0) {
      NULL
    } else {
      rep(NA_character_, length(constructors_list()))
    }
    if (!is.null(preds$drivers)) {
      walk(
        seq_along(preds$drivers),
        ~ updateSelectInput(
          session,
          paste0("driver_rank_", .x),
          selected = NA_character_
        )
      )
    }
    if (!is.null(preds$teams)) {
      walk(
        seq_along(preds$teams),
        ~ updateSelectInput(
          session,
          paste0("team_rank_", .x),
          selected = NA_character_
        )
      )
    }
    updateSelectInput(session, "most_podium_driver", selected = NA_character_)
    updateSelectInput(session, "most_podium_team", selected = NA_character_)
  })

  observe({
    drv <- drivers_list()
    if (length(drv) == 0) {
      return()
    }
    walk(seq_along(drv), function(i) {
      input_id <- paste0("driver_rank_", i)
      observeEvent(
        input[[input_id]],
        {
          preds$drivers[[i]] <- input[[input_id]]
        },
        ignoreInit = TRUE
      )
    })
  })

  observe({
    tm <- constructors_list()
    if (length(tm) == 0) {
      return()
    }
    walk(seq_along(tm), function(i) {
      input_id <- paste0("team_rank_", i)
      observeEvent(
        input[[input_id]],
        {
          preds$teams[[i]] <- input[[input_id]]
        },
        ignoreInit = TRUE
      )
    })
  })

  output$driver_standings_table <- renderTable(
    {
      driver_standings()
    },
    digits = 2,
    striped = TRUE,
    bordered = TRUE
  )

  output$constructor_standings_table <- renderTable(
    {
      constructor_standings()
    },
    digits = 2,
    striped = TRUE,
    bordered = TRUE
  )

  driver_points_table <- reactive({
    res <- season_results()
    sched <- schedule_data()
    if (nrow(res) == 0 || nrow(sched) == 0) {
      return(tibble())
    }
    res |>
      mutate(round = as.integer(round)) |>
      left_join(sched |> select(round, country), by = "round") |>
      mutate(col = paste0(sprintf("R%02d ", round), flag_badge(country))) |>
      select(driver, col, points) |>
      mutate(points = if_else(is.na(points), "", as.character(points))) |>
      distinct() |>
      pivot_wider(names_from = col, values_from = points, values_fn = list) |>
      arrange(driver)
  })

  constructor_points_table <- reactive({
    res <- season_results()
    sched <- schedule_data()
    if (nrow(res) == 0 || nrow(sched) == 0) {
      return(tibble())
    }
    res |>
      mutate(round = as.integer(round)) |>
      left_join(sched |> select(round, country), by = "round") |>
      group_by(constructor, round, country) |>
      summarise(points = sum(points, na.rm = TRUE), .groups = "drop") |>
      mutate(
        points = if_else(points == 0, "", as.character(points)),
        col = paste0(sprintf("R%02d ", round), flag_badge(country))
      ) |>
      select(constructor, col, points) |>
      distinct() |>
      pivot_wider(names_from = col, values_from = points, values_fn = list) |>
      arrange(constructor)
  })

  output$driver_points <- renderTable(
    {
      driver_points_table()
    },
    sanitize.text.function = identity,
    bordered = TRUE,
    striped = TRUE
  )

  output$team_points <- renderTable(
    {
      constructor_points_table()
    },
    sanitize.text.function = identity,
    bordered = TRUE,
    striped = TRUE
  )
}

shinyApp(ui, server)

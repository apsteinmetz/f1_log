library(shiny)
library(bslib)
library(tidyverse)
library(httr2)
library(lubridate)
library(leaflet)
library(glue)

season_year <- 2026
base_url <- "https://api.jolpi.ca/ergast/f1"
predictions_file <- "data/predictions.rds"

country_iso_lookup <- c(
  "Abu Dhabi" = "ae",
  "UAE" = "ae",
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
      circuit_url = r$Circuit$url %||% NA_character_,
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

get_previous_winner <- function(circuit_name) {
  # Fetch 2025 schedule to find the round for this circuit
  sched_2025 <- get_schedule(2025)
  if (nrow(sched_2025) == 0) {
    return(list(driver = NA_character_, team = NA_character_))
  }

  # Find the matching circuit in 2025
  match_row <- sched_2025 |>
    filter(circuit == circuit_name) |>
    slice_head(n = 1)
  if (nrow(match_row) == 0) {
    return(list(driver = NA_character_, team = NA_character_))
  }

  # Get results for that round
  results <- get_results(2025, match_row$round[[1]])
  if (nrow(results) == 0) {
    return(list(driver = NA_character_, team = NA_character_))
  }

  winner <- results |> filter(position == 1) |> slice_head(n = 1)
  if (nrow(winner) == 0) {
    return(list(driver = NA_character_, team = NA_character_))
  }

  list(driver = winner$driver[[1]], team = winner$constructor[[1]])
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

get_sprint <- function(season, round) {
  resp <- fetch_json(glue("{season}/{round}/sprint"), limit = 200)
  if (is.null(resp) || is.null(resp$MRData$RaceTable$Races)) {
    return(tibble())
  }
  races <- resp$MRData$RaceTable$Races %||% list()
  if (length(races) == 0 || is.null(races[[1]]$SprintResults)) {
    return(tibble())
  }
  map_dfr(races[[1]]$SprintResults, function(x) {
    tibble(
      position = as.integer(x$position %||% NA_integer_),
      driver = paste(x$Driver$givenName, x$Driver$familyName),
      constructor = x$Constructor$name %||% "",
      points = as.numeric(x$points %||% NA_real_),
      status = x$status %||% ""
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
    card(
      card_body(
        class = "py-2",
        layout_columns(
          col_widths = c(4, 4, 4),
          textInput("user_name", "Your Name", placeholder = "Enter your name"),
          actionButton(
            "load_predictions",
            "Load My Predictions",
            class = "btn-primary mt-4"
          ),
          actionButton(
            "save_predictions",
            "Save My Predictions",
            class = "btn-success mt-4"
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(3, 2, 3, 2, 2),
      card(
        card_header("Driver Predictions"),
        card_body(
          uiOutput("driver_predictions"),
          actionButton(
            "clear_driver_predictions",
            "Clear",
            class = "btn-secondary btn-sm mt-2"
          )
        )
      ),
      card(
        card_header("Driver Standings"),
        card_body(
          tableOutput("driver_standings_table")
        )
      ),
      card(
        card_header("Team Predictions"),
        card_body(
          uiOutput("team_predictions"),
          actionButton(
            "clear_team_predictions",
            "Clear",
            class = "btn-secondary btn-sm mt-2"
          )
        )
      ),
      card(
        card_header("Constructor Standings"),
        card_body(
          tableOutput("constructor_standings_table")
        )
      ),
      card(
        card_header("Podium Predictions"),
        card_body(
          div(
            class = "d-flex align-items-center mb-2",
            tags$label("Driver", class = "me-2", style = "min-width: 50px;"),
            div(
              style = "flex: 1;",
              selectInput(
                "most_podium_driver",
                NULL,
                choices = NULL,
                width = "100%"
              )
            )
          ),
          div(
            class = "d-flex align-items-center mb-2",
            tags$label("Team", class = "me-2", style = "min-width: 50px;"),
            div(
              style = "flex: 1;",
              selectInput(
                "most_podium_team",
                NULL,
                choices = NULL,
                width = "100%"
              )
            )
          ),
          actionButton(
            "clear_podium_predictions",
            "Clear",
            class = "btn-secondary btn-sm mt-2"
          )
        )
      )
    )
  ),
  nav_panel(
    "Driver Points",
    card(
      card_body(
        style = "overflow-x: auto;",
        uiOutput("driver_points")
      )
    )
  ),
  nav_panel(
    "Team Points",
    card(
      card_body(
        style = "overflow-x: auto;",
        uiOutput("team_points")
      )
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

  prev_winner <- reactive({
    info <- race_info()
    if (nrow(info) == 0) {
      return(list(driver = NA_character_, team = NA_character_))
    }
    get_previous_winner(info$circuit[[1]])
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

  sprint_data <- reactive({
    if (!race_past()) {
      return(tibble())
    }
    get_sprint(season_year, selected_round())
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

  output$sprint_table <- renderTable(
    {
      sprint_data()
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
      sprint <- sprint_data()
      has_sprint <- nrow(sprint) > 0

      # Build cards list dynamically to avoid gaps
      prev <- prev_winner()
      winner_text <- if (!is.na(prev$driver) && !is.na(prev$team)) {
        paste0("2025 Winner: ", prev$driver, " (", prev$team, ")")
      } else {
        NULL
      }

      cards_list <- list(
        card(
          card_header(paste("Round", info$round[[1]], "-", info$race[[1]])),
          card_body(
            p(flag_badge(info$country[[1]])),
            p(
              tags$a(
                href = info$circuit_url[[1]],
                target = "_blank",
                info$circuit[[1]]
              ),
              if (!is.null(winner_text)) tags$br(),
              if (!is.null(winner_text)) {
                tags$small(class = "text-muted", winner_text)
              }
            ),
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
        )
      )

      if (has_sprint) {
        cards_list <- c(
          cards_list,
          list(
            card(
              card_header("Sprint"),
              card_body(
                tableOutput("sprint_table")
              )
            )
          )
        )
      }

      cards_list <- c(
        cards_list,
        list(
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

      tagList(
        div(
          class = "mb-3",
          actionButton(
            "close_race",
            "← Back to Schedule",
            class = "btn-secondary"
          )
        ),
        do.call(
          layout_columns,
          c(
            list(
              col_widths = if (has_sprint) c(3, 3, 2, 2, 2) else c(4, 3, 3, 2)
            ),
            cards_list
          )
        )
      )
    }
  })

  preds <- reactiveValues(
    drivers = NULL,
    teams = NULL,
    podium_driver = NA_character_,
    podium_team = NA_character_
  )

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
      div(
        class = "d-flex align-items-center",
        style = "margin-bottom: -10px;",
        tags$span(
          paste0("P", i),
          class = "me-2",
          style = "min-width: 30px; font-weight: 500;"
        ),
        div(
          style = "flex: 1;",
          selectInput(
            inputId = paste0("driver_rank_", i),
            label = NULL,
            choices = available_choice(drv, preds$drivers, i),
            selected = preds$drivers[[i]],
            width = "100%"
          )
        )
      )
    })
  })

  output$team_predictions <- renderUI({
    tm <- constructors_list()
    if (length(tm) == 0) {
      return(tags$p("Constructors unavailable."))
    }
    lapply(seq_along(tm), function(i) {
      div(
        class = "d-flex align-items-center",
        style = "margin-bottom: -10px;",
        tags$span(
          paste0("P", i),
          class = "me-2",
          style = "min-width: 30px; font-weight: 500;"
        ),
        div(
          style = "flex: 1;",
          selectInput(
            inputId = paste0("team_rank_", i),
            label = NULL,
            choices = available_choice(tm, preds$teams, i),
            selected = preds$teams[[i]],
            width = "100%"
          )
        )
      )
    })
  })

  observeEvent(input$clear_driver_predictions, {
    preds$drivers <- if (length(drivers_list()) == 0) {
      NULL
    } else {
      rep(NA_character_, length(drivers_list()))
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
  })

  observeEvent(input$clear_team_predictions, {
    preds$teams <- if (length(constructors_list()) == 0) {
      NULL
    } else {
      rep(NA_character_, length(constructors_list()))
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
  })

  observeEvent(input$clear_podium_predictions, {
    updateSelectInput(session, "most_podium_driver", selected = NA_character_)
    updateSelectInput(session, "most_podium_team", selected = NA_character_)
    preds$podium_driver <- NA_character_
    preds$podium_team <- NA_character_
  })

  # Track podium selections
  observeEvent(input$most_podium_driver, {
    preds$podium_driver <- input$most_podium_driver
  })
  observeEvent(input$most_podium_team, {
    preds$podium_team <- input$most_podium_team
  })

  # Save predictions to file
  observeEvent(input$save_predictions, {
    req(input$user_name)
    user_name <- trimws(input$user_name)
    if (nchar(user_name) == 0) {
      showNotification("Please enter your name.", type = "warning")
      return()
    }

    # Load existing predictions or create new list
    all_preds <- if (file.exists(predictions_file)) {
      readRDS(predictions_file)
    } else {
      list()
    }

    # Save this user's predictions
    all_preds[[user_name]] <- list(
      drivers = preds$drivers,
      teams = preds$teams,
      podium_driver = preds$podium_driver,
      podium_team = preds$podium_team,
      timestamp = Sys.time()
    )

    saveRDS(all_preds, predictions_file)
    showNotification(
      paste("Predictions saved for", user_name),
      type = "message"
    )
  })

  # Load predictions from file
  observeEvent(input$load_predictions, {
    req(input$user_name)
    user_name <- trimws(input$user_name)
    if (nchar(user_name) == 0) {
      showNotification("Please enter your name.", type = "warning")
      return()
    }

    if (!file.exists(predictions_file)) {
      showNotification("No saved predictions found.", type = "warning")
      return()
    }

    all_preds <- readRDS(predictions_file)
    if (!user_name %in% names(all_preds)) {
      showNotification(
        paste("No predictions found for", user_name),
        type = "warning"
      )
      return()
    }

    user_preds <- all_preds[[user_name]]

    # Restore driver predictions
    if (!is.null(user_preds$drivers)) {
      preds$drivers <- user_preds$drivers
      walk(seq_along(user_preds$drivers), function(i) {
        updateSelectInput(
          session,
          paste0("driver_rank_", i),
          selected = user_preds$drivers[[i]]
        )
      })
    }

    # Restore team predictions
    if (!is.null(user_preds$teams)) {
      preds$teams <- user_preds$teams
      walk(seq_along(user_preds$teams), function(i) {
        updateSelectInput(
          session,
          paste0("team_rank_", i),
          selected = user_preds$teams[[i]]
        )
      })
    }

    # Restore podium predictions
    preds$podium_driver <- user_preds$podium_driver
    preds$podium_team <- user_preds$podium_team
    updateSelectInput(
      session,
      "most_podium_driver",
      selected = user_preds$podium_driver
    )
    updateSelectInput(
      session,
      "most_podium_team",
      selected = user_preds$podium_team
    )

    showNotification(
      paste("Predictions loaded for", user_name),
      type = "message"
    )
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
      driver_standings() |>
        select(Pos = position, Driver = driver)
    },
    striped = TRUE,
    bordered = TRUE
  )

  output$constructor_standings_table <- renderTable(
    {
      constructor_standings() |>
        select(Pos = position, Constructor = constructor)
    },
    striped = TRUE,
    bordered = TRUE
  )

  # Helper to build flag image HTML string
  flag_img <- function(country, size = 24) {
    if (is.null(country) || is.na(country) || country == "") {
      return("")
    }
    code <- unname(country_iso_lookup[country])
    if (is.na(code) || is.null(code)) {
      return(country)
    }
    height <- round(size * 3 / 4)
    glue(
      '<img src="https://flagcdn.com/{size}x{height}/{tolower(code)}.png" width="{size}" height="{height}" alt="{country}" title="{country}">'
    )
  }

  output$driver_points <- renderUI({
    res <- season_results()
    sched <- schedule_data()

    if (nrow(sched) == 0) {
      return(tags$p("Schedule not available."))
    }

    # Get all drivers from results or standings
    all_drivers <- driver_standings()
    if (nrow(all_drivers) == 0) {
      return(tags$p("Driver data not available."))
    }
    drivers <- all_drivers$driver

    # Build points matrix: rows = drivers, cols = rounds
    sched <- sched |> arrange(round)
    rounds <- sched$round
    countries <- sched$country

    # Create header row with flags
    header_cells <- list(
      tags$th("Driver"),
      tags$th(style = "text-align: center;", "Total")
    )
    for (i in seq_along(rounds)) {
      header_cells <- c(
        header_cells,
        list(tags$th(
          style = "text-align: center; min-width: 50px;",
          HTML(flag_img(countries[i], size = 32))
        ))
      )
    }

    # Create data rows
    body_rows <- lapply(drivers, function(drv) {
      total_pts <- res |>
        filter(driver == drv) |>
        summarise(total = sum(points, na.rm = TRUE)) |>
        pull(total)
      total_val <- if (is.na(total_pts) || total_pts == 0) {
        ""
      } else {
        as.character(total_pts)
      }
      cells <- list(
        tags$td(style = "white-space: nowrap;", drv),
        tags$td(style = "text-align: center; font-weight: bold;", total_val)
      )
      for (i in seq_along(rounds)) {
        rnd <- rounds[i]
        pts <- res |>
          filter(driver == drv, round == as.character(rnd)) |>
          pull(points)
        pts_val <- if (length(pts) == 0 || is.na(pts[1])) {
          ""
        } else {
          as.character(pts[1])
        }
        cells <- c(cells, list(tags$td(style = "text-align: center;", pts_val)))
      }
      tags$tr(cells)
    })

    tags$table(
      class = "table table-striped table-bordered table-sm",
      tags$thead(tags$tr(header_cells)),
      tags$tbody(body_rows)
    )
  })

  output$team_points <- renderUI({
    res <- season_results()
    sched <- schedule_data()

    if (nrow(sched) == 0) {
      return(tags$p("Schedule not available."))
    }

    # Get all constructors from standings
    all_constructors <- constructor_standings()
    if (nrow(all_constructors) == 0) {
      return(tags$p("Constructor data not available."))
    }
    constructors <- all_constructors$constructor

    # Aggregate points by constructor and round
    team_pts <- res |>
      group_by(constructor, round) |>
      summarise(points = sum(points, na.rm = TRUE), .groups = "drop")

    # Build points matrix
    sched <- sched |> arrange(round)
    rounds <- sched$round
    countries <- sched$country

    # Create header row with flags
    header_cells <- list(
      tags$th("Team"),
      tags$th(style = "text-align: center;", "Total")
    )
    for (i in seq_along(rounds)) {
      header_cells <- c(
        header_cells,
        list(tags$th(
          style = "text-align: center; min-width: 50px;",
          HTML(flag_img(countries[i], size = 32))
        ))
      )
    }

    # Create data rows
    body_rows <- lapply(constructors, function(team) {
      total_pts <- team_pts |>
        filter(constructor == team) |>
        summarise(total = sum(points, na.rm = TRUE)) |>
        pull(total)
      total_val <- if (is.na(total_pts) || total_pts == 0) {
        ""
      } else {
        as.character(total_pts)
      }
      cells <- list(
        tags$td(style = "white-space: nowrap;", team),
        tags$td(style = "text-align: center; font-weight: bold;", total_val)
      )
      for (i in seq_along(rounds)) {
        rnd <- rounds[i]
        pts <- team_pts |>
          filter(constructor == team, round == as.character(rnd)) |>
          pull(points)
        pts_val <- if (length(pts) == 0 || is.na(pts[1]) || pts[1] == 0) {
          ""
        } else {
          as.character(pts[1])
        }
        cells <- c(cells, list(tags$td(style = "text-align: center;", pts_val)))
      }
      tags$tr(cells)
    })

    tags$table(
      class = "table table-striped table-bordered table-sm",
      tags$thead(tags$tr(header_cells)),
      tags$tbody(body_rows)
    )
  })
}

shinyApp(ui, server)

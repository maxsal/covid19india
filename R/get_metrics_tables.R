#' Create metrics tables
#' @param seed set seed
#' @param top20 Vector of state abbreviations for top 20 table
#' @param corr_check Check for data corrections of X-times magnitude. Default is `TRUE`
#' @return Creates metrics tables for use in covind19.org
#' @import gt
#' @import data.table
#' @import cli
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom scales col_bin
#' @export
#' @examples
#' \dontrun{
#' tabs   <- get_metrics_tables()
#'
#' tabs$full
#' }

get_metrics_tables <- function(seed = 46342, top20 = NULL, corr_check = FALSE) {

  cli::cli_alert_info("getting data...")

  set.seed(set_seed <- seed)
  today           <- Sys.Date()
  all_data <- get_all_data(corr_check = corr_check)[date <= today]
  # dat             <- get_all_data(corr_check = corr_check)[date <= today]
  cfr1            <- unique(get_cfr(all_data))[place == "National estimate", place := "India"][]
  r_est           <- get_r_est(all_data[!is.na(r_est)])
  india_state_pop <- covid19india::pop

  cli::cli_alert_success("data load success!!")

  # pull abbrevs -----------
  use_abbrevs <- tolower(unique(all_data[abbrev != "la", abbrev]))

  # vax data ----------
  vax_dat <- get_state_vax()[date <= today]
  setnames(vax_dat, c("total_doses", "pct_one_dose", "pct_two_doses", "daily_doses"), c("total_vacc", "pct_at_least_one", "pct_second", "daily_vax_dose"))

  test_data = (((fread("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/source_data/count_test_vax_latest.csv") %>%
    as.data.table())[,
      .SD[date >= max(date) - 8 & date < max(date)], by = "place"
      ][,
      .SD, .SDcols = c("state", "date", "confirmed", "tested")
      ][
      , daily_confirmed := confirmed - shift(confirmed), by = "state"
      ][
      , daily_tested := tested - shift(tested), by = "state"
      ][
      , daily_tested := ifelse(daily_tested == 0 , NA, daily_tested), by = "state"
      ] %>%
    na.omit(cols = c("daily_confirmed")))[
      , tpr7d := mean(daily_confirmed / daily_tested, na.rm = TRUE), by = "state"
      ][,
      .SD, .SDcols = c("state", "tpr7d", "tested")
      ][
      , .SD[nrow(.SD)],by = "state"
      ][
      , tpr7d := ifelse(is.nan(tpr7d), NA, tpr7d)
      ]) %>%
    data.table::setnames(old = c("state"), new = c("place"))

  vax_dat <- vax_dat[!is.na(total_vacc)][, .SD[date == max(date)], by = "place"]

  all_data <- unique(all_data[, .SD[date > max(as.Date(date) - 7)], by = "place"][, dailyCFR7 := daily_deaths / daily_cases][, dailyCFR7d := mean(dailyCFR7, na.rm = T), by = "place"][, .SD[date == max(date)], by = "place"][])[, .(place, dailyCFR7d, daily_cases, daily_deaths, total_cases, total_deaths)]

  all_data <- data.table::merge.data.table(all_data, covid19india::pop[, .SD[1], by = "place"], by = "place", all.x = TRUE)

  all_data <- data.table::merge.data.table(all_data, vax_dat, by = "place", all.x = TRUE)

  all_data <- data.table::merge.data.table(all_data, test_data, by = "place")

  # table ----------
  tib <- cfr1[, .(place, cfr)]

  tib <- data.table::merge.data.table(tib, r_est[, .(place, r)], by = "place", all.x = TRUE)

  # tib <- data.table::merge.data.table(tib, extract_latest(tp, clmns = c("tpr")), by = "place", all.x = TRUE)
  tib <- data.table::merge.data.table(tib, all_data[, .(place, tpr7d)], by = "place", all.x = TRUE)

  tib <- data.table::merge.data.table(tib, all_data[, tpr7d := NULL], by = "place", all.x = TRUE)[
    , `:=` (
      perc_vaccine   = pct_at_least_one,
      total_vacc     = format(total_vacc, big.mark = ","),
      daily_cases    = format(daily_cases, big.mark = ","),
      daily_deaths   = format(daily_deaths, big.mark = ","),
      daily_vax_dose = format(daily_vax_dose, big.mark = ","),
      cases          = format(total_cases, big.mark = ","),
      deaths         = format(total_deaths, big.mark = ","),
      tested         = format(tested, big.mark = ",")
    )
  ][]

  setnames(tib,
           old = c( "daily_cases", "daily_deaths", "dailyCFR7d", "r", "tpr7d", "daily_vax_dose", "place", "cfr", "cases", "deaths", "perc_vaccine", "total_vacc", "pct_second", "pct_at_least_one", "tested"),
           new = c("# daily new cases", "# daily new deaths",  "7-day average daily CFR", "R", "7-day average daily TPR", "daily vaccine doses", "Location", "CFR", "total cases","total deaths",  "Percent with at least one dose", "Total doses", "% pop. with two shots", "% pop. with at least one shot", "total tests"))

  tib <- tib[order(-`total_cases`)][
    , `:=` (
      `7-day average daily CFR`       = round(`7-day average daily CFR`, digits = 3),
      `7-day average daily TPR (%)`       = round(`7-day average daily TPR`, digits = 6)*100,
      `% pop. with two shots`         = round(`% pop. with two shots`, digits = 2),
      `% pop. with at least one shot` = round(`% pop. with at least one shot`, digits = 2)
    )
  ][
    , .(`# daily new cases`, `# daily new deaths`, `7-day average daily CFR`,
        `7-day average daily TPR (%)`,
        Location, R, `daily vaccine doses`, CFR, `total tests`, `total cases`,
        `total deaths`, `Total doses`, `% pop. with two shots`,
        `% pop. with at least one shot`)
  ]

  tib <- unique(tib)[!grepl("\\*\\*", tib$Location),]

  source_note_text <- glue::glue(
    "**\uA9 COV-IND-19 Study Group**<br>**Source data:** Up to 10/17/2021: covid19india.org. After 10/17/2021: count data (mohfw.gov.in), vaccine data (cowin.gov.in)<br>
      **Notes:** Cells highlighted in green indicates good performance for given metric while red indicates need for improvement.
      Only states/union territories with the highest cumulative case counts as of {format(today, '%B %e')} are shown.
      States are omitted if they have missing case count data.
      <br>
      R values are not reliable when case counts are below 100.
      <br>
      **Abbrev:** CFR, Case-fatality rate."
  )

  tabl <- tib %>%
    gt() %>%
    # format table body text
    tab_style(
      style     = cell_text(size = px(14), font = "helvetica"),
      locations = cells_body()
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body((Location))
    ) %>%
    # format column names
    tab_style(
      style = cell_text(
        size      = px(12),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    ) %>%
    # format numbers
    fmt_number(
      columns  = c(CFR),
      decimals = 3
    ) %>%
    fmt_number(
      columns  = c(R),
      decimals = 2
    ) %>%
    fmt_number(
      columns  = c(`7-day average daily TPR (%)`),
      decimals = 3
    ) %>%
    # random formatting
    tab_options(
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#334422",
      table_body.border.bottom.color    = "#0000001A",
      data_row.padding                  = px(4)
    ) %>%
    # column widths
    cols_width(
      Location ~ px(150),
      c(R, CFR) ~ px(75),
      everything() ~ px(100)
    ) %>%
    cols_align(
      align   = "center",
      columns = everything()
    ) %>%
    # title
    tab_header(
      title    = md("**Assessing COVID-19 in India**"),
      subtitle = glue("data through {format(today, '%B %e')}")
    ) %>%
    # caption
    tab_source_note(
      source_note = md(source_note_text)
    ) %>%
    # add and format column spanners
    tab_spanner(
      label   = "Point in time metrics",
      columns = c(`# daily new cases`, `# daily new deaths`,
                  `7-day average daily CFR`, R, `7-day average daily TPR (%)`, `daily vaccine doses`)
    ) %>%
    tab_spanner(
      label   = "Cumulative metrics",
      columns = c(`total cases`, `total deaths`, CFR, `total tests`,
                  `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
    ) %>%
    cols_move_to_start((Location)) %>%
    tab_style(
      style = cell_text(
        size      = px(14),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_spanners(spanners = c("Point in time metrics", "Cumulative metrics"))
    ) %>%
    # adjust title font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(24))),
      locations = list(cells_title(groups = "title"))
    ) %>%
    # adjust subtitle font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(18))),
      locations = list(cells_title(groups = "subtitle"))
    ) %>%
    # color cells based on values
    data_color(
      columns = c(R),
      colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
    ) %>%
    # highlight national estimate
    tab_style(
      style = cell_fill(color = "#fcf8d4"),
      locations = cells_body(
        rows = Location == "India")
    ) %>%
    tab_style(
      style = cell_borders(sides = "left"),
      locations = cells_body(columns = (`total cases`))
    ) %>%
    tab_style(
      style = cell_borders(sides = "left"),
      locations = cells_column_labels(columns = (`total cases`))
    ) %>%
    tab_style(
      style = cell_borders(sides = "left"),
      locations = cells_column_spanners(("Cumulative metrics"))
    )

  cli::cli_alert_success("full table made")

  # new table
  point_in_time <- tib[, !c("total cases", "total deaths", "CFR",
                            "Total doses", "% pop. with two shots",
                            "% pop. with at least one shot")] %>%
    gt() %>%
    # format table body text
    tab_style(
      style     = cell_text(size = px(14), font = "helvetica"),
      locations = cells_body()
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(c(Location))
    ) %>%
    # format column names
    tab_style(
      style = cell_text(
        size      = px(12),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    ) %>%
    # format numbers
    # fmt_number(
    #   # columns  = c(`7-day average daily TPR`),
    #   decimals = 3
    # ) %>%
    fmt_number(
      columns  = c(R),
      decimals = 2
    ) %>%
    # random formatting
    tab_options(
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#334422",
      table_body.border.bottom.color    = "#0000001A",
      data_row.padding                  = px(4)
    ) %>%
    # column widths
    cols_width(
      Location ~ px(150),
      R ~ px(75),
      everything() ~ px(100)
    ) %>%
    cols_align(
      align   = "center",
      columns = everything()
    ) %>%
    # title
    tab_header(
      title    = md("**Assessing COVID-19 in India**"),
      subtitle = glue("data through {format(today, '%B %e')}")
    ) %>%
    # caption
    tab_source_note(
      source_note = md(source_note_text)
    ) %>%
    tab_spanner(
      label   = "Point in time metrics",
      columns = c(`# daily new cases`, `# daily new deaths`,
                  `7-day average daily CFR`, R, `daily vaccine doses`)
    ) %>%
    cols_move_to_start((Location)) %>%
    tab_style(
      style = cell_text(
        size      = px(14),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_spanners(spanners = c("Point in time metrics")) #, glue("Predictions on ({format(today + 21, '%m/%d')}) (No intervention)")
    ) %>%
    # adjust title font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(24))),
      locations = list(cells_title(groups = "title"))
    ) %>%
    # adjust subtitle font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(18))),
      locations = list(cells_title(groups = "subtitle"))
    ) %>%
    # color cells based on values
    data_color(
      columns = c(R),
      colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
    ) %>%
    # data_color(
    #   columns = c(`7-day average daily TPR`),
    #   colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.05, 1), pretty = F, na.color = "#e8e8e8")
    # ) %>%
    # highlight national estimate
    tab_style(
      style = cell_fill(color = "#fcf8d4"),
      locations = cells_body(
        rows = Location == "India")
    )

  cli::cli_alert_success("point-in-time table made")

  cumulative <- tib[, !c("# daily new cases", "# daily new deaths",
                         "7-day average daily CFR", "R", "daily vaccine doses")] %>%
    gt() %>%
    # format table body text
    tab_style(
      style     = cell_text(size = px(14), font = "helvetica"),
      locations = cells_body()
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(c(Location))
    ) %>%
    # format column names
    tab_style(
      style = cell_text(
        size      = px(12),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_labels(everything())
    ) %>%
    # format numbers
    fmt_number(
      columns  = c(CFR),
      decimals = 3
    ) %>%
    # random formatting
    tab_options(
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#334422",
      table_body.border.bottom.color    = "#0000001A",
      data_row.padding                  = px(4)
    ) %>%
    # column widths
    cols_width(
      (Location) ~ px(150),
      (CFR) ~ px(75),
      everything() ~ px(100)
    ) %>%
    cols_align(
      align   = "center",
      columns = everything()
    ) %>%
    # title
    tab_header(
      title    = md("**Assessing COVID-19 in India**"),
      subtitle = glue("data through {format(today, '%B %e')}")
    ) %>%
    # caption
    tab_source_note(
      source_note = md(source_note_text)
    ) %>%
    tab_spanner(
      label   = "Cumulative metrics",
      columns = c(`total cases`, `total deaths`, CFR,
                  `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
    ) %>%
    cols_move_to_start((Location)) %>%
    tab_style(
      style = cell_text(
        size      = px(14),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = cells_column_spanners(spanners = c("Cumulative metrics"))
    ) %>%
    # adjust title font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(24))),
      locations = list(cells_title(groups = "title"))
    ) %>%
    # adjust subtitle font
    tab_style(
      style     = list(cell_text(font = "helvetica", size = px(18))),
      locations = list(cells_title(groups = "subtitle"))
    ) %>%
    # highlight national estimate
    tab_style(
      style = cell_fill(color = "#fcf8d4"),
      locations = cells_body(rows = Location == "India")
    )

  cli::cli_alert_success("cumulative table made")

  if (!is.null(top20)) {

    t20_tib <- data.table::merge.data.table(tib, covid19india::pop[, !c("population")], by.x = "Location", by.y = "place")[abbrev %in% unique(c(top20, "tt"))][, !c("abbrev")][order(-`total cases`)]

    source_note_text <- glue(
      "**\uA9 COV-IND-19 Study Group**<br>**Source data:** Up to 10/17/2021: covid19india.org. After 10/17/2021: count data (mohfw.gov.in), vaccine data (cowin.gov.in)<br>
      **Notes:** Cells highlighted in green indicates good performance for given metric while red indicates need for improvement.
      Only states/union territories with the highest cumulative case counts as of {format(today, '%B %e')} are shown.
      States are omitted if they have missing case count data.
      <br>
      R values are not reliable when case counts are below 100.
      <br>
      **Abbrev:** CFR, Case-fatality rate."
    )

    t20_tabl <- t20_tib %>%
      gt() %>%
      # format table body text
      tab_style(
        style     = cell_text(size = px(14), font = "helvetica"),
        locations = cells_body()
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body((Location))
      ) %>%
      # format column names
      tab_style(
        style = cell_text(
          size      = px(12),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      # format numbers
      fmt_number(
        columns  = c(CFR),
        decimals = 3
      ) %>%
      fmt_number(
        columns  = c(R),
        decimals = 2
      ) %>%
      # random formatting
      tab_options(
        column_labels.border.top.style    = "none",
        column_labels.border.bottom.width = 1,
        column_labels.border.bottom.color = "#334422",
        table_body.border.bottom.color    = "#0000001A",
        data_row.padding                  = px(4)
      ) %>%
      # column widths
      cols_width(
        Location ~ px(150),
        c(R, CFR) ~ px(75),
        everything() ~ px(100)
      ) %>%
      cols_align(
        align   = "center",
        columns = everything()
      ) %>%
      # title
      tab_header(
        title    = md("**Assessing COVID-19 in India**"),
        subtitle = glue("data through {format(today, '%B %e')}")
      ) %>%
      # caption
      tab_source_note(
        source_note = md(source_note_text)
      ) %>%
      # add and format column spanners
      tab_spanner(
        label   = "Point in time metrics",
        columns = c(`# daily new cases`, `# daily new deaths`,
                    `7-day average daily CFR`, R, `daily vaccine doses`)
      ) %>%
      tab_spanner(
        label   = "Cumulative metrics",
        columns = c(`total cases`, `total deaths`, CFR,
                    `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
      ) %>%
      cols_move_to_start((Location)) %>%
      tab_style(
        style = cell_text(
          size      = px(14),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_spanners(spanners = c("Point in time metrics", "Cumulative metrics"))
      ) %>%
      # adjust title font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(24))),
        locations = list(cells_title(groups = "title"))
      ) %>%
      # adjust subtitle font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(18))),
        locations = list(cells_title(groups = "subtitle"))
      ) %>%
      # color cells based on values
      data_color(
        columns = c(R),
        colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
      ) %>%
      # highlight national estimate
      tab_style(
        style = cell_fill(color = "#fcf8d4"),
        locations = cells_body(
          rows = Location == "India")
      ) %>%
      tab_style(
        style = cell_borders(sides = "left"),
        locations = cells_body(columns = (`total cases`))
      ) %>%
      tab_style(
        style = cell_borders(sides = "left"),
        locations = cells_column_labels(columns = (`total cases`))
      ) %>%
      tab_style(
        style = cell_borders(sides = "left"),
        locations = cells_column_spanners(("Cumulative metrics"))
      )

    cli::cli_alert_success("full top 20 table made")

    # new table
    t20_point_in_time <- t20_tib[, !c("total cases", "total deaths", "CFR",
                                      "Total doses", "% pop. with two shots",
                                      "% pop. with at least one shot")] %>%
      gt() %>%
      # format table body text
      tab_style(
        style     = cell_text(size = px(14), font = "helvetica"),
        locations = cells_body()
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(c(Location))
      ) %>%
      # format column names
      tab_style(
        style = cell_text(
          size      = px(12),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      # format numbers
      fmt_number(
        columns  = c(R),
        decimals = 2
      ) %>%
      # random formatting
      tab_options(
        column_labels.border.top.style    = "none",
        column_labels.border.bottom.width = 1,
        column_labels.border.bottom.color = "#334422",
        table_body.border.bottom.color    = "#0000001A",
        data_row.padding                  = px(4)
      ) %>%
      # column widths
      cols_width(
        Location ~ px(150),
        R ~ px(75),
        everything() ~ px(100)
      ) %>%
      cols_align(
        align   = "center",
        columns = everything()
      ) %>%
      # title
      tab_header(
        title    = md("**Assessing COVID-19 in India**"),
        subtitle = glue("data through {format(today, '%B %e')}")
      ) %>%
      # caption
      tab_source_note(
        source_note = md(source_note_text)
      ) %>%
      tab_spanner(
        label   = "Point in time metrics",
        columns = c(`# daily new cases`, `# daily new deaths`,
                    `7-day average daily CFR`, R, `daily vaccine doses`)
      ) %>%
      cols_move_to_start((Location)) %>%
      tab_style(
        style = cell_text(
          size      = px(14),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_spanners(spanners = c("Point in time metrics")) #, glue("Predictions on ({format(today + 21, '%m/%d')}) (No intervention)")
      ) %>%
      # adjust title font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(24))),
        locations = list(cells_title(groups = "title"))
      ) %>%
      # adjust subtitle font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(18))),
        locations = list(cells_title(groups = "subtitle"))
      ) %>%
      # color cells based on values
      data_color(
        columns = c(R),
        colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
      ) %>%
      # highlight national estimate
      tab_style(
        style = cell_fill(color = "#fcf8d4"),
        locations = cells_body(
          rows = Location == "India")
      )

    cli::cli_alert_success("top 20 point-in-time table made")

    t20_cumulative <- t20_tib[, !c("# daily new cases", "# daily new deaths",
                                   "7-day average daily CFR", "R", "daily vaccine doses")] %>%
      gt() %>%
      # format table body text
      tab_style(
        style     = cell_text(size = px(14), font = "helvetica"),
        locations = cells_body()
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(c(Location))
      ) %>%
      # format column names
      tab_style(
        style = cell_text(
          size      = px(12),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      # format numbers
      fmt_number(
        columns  = c(CFR),
        decimals = 3
      ) %>%
      # random formatting
      tab_options(
        column_labels.border.top.style    = "none",
        column_labels.border.bottom.width = 1,
        column_labels.border.bottom.color = "#334422",
        table_body.border.bottom.color    = "#0000001A",
        data_row.padding                  = px(4)
      ) %>%
      # column widths
      cols_width(
        (Location) ~ px(150),
        (CFR) ~ px(75),
        everything() ~ px(100)
      ) %>%
      cols_align(
        align   = "center",
        columns = everything()
      ) %>%
      # title
      tab_header(
        title    = md("**Assessing COVID-19 in India**"),
        subtitle = glue("data through {format(today, '%B %e')}")
      ) %>%
      # caption
      tab_source_note(
        source_note = md(source_note_text)
      ) %>%
      tab_spanner(
        label   = "Cumulative metrics",
        columns = c(`total cases`, `total deaths`, CFR,
                    `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
      ) %>%
      cols_move_to_start((Location)) %>%
      tab_style(
        style = cell_text(
          size      = px(14),
          color     = "#999999",
          font      = "helvetica",
          transform = "uppercase"
        ),
        locations = cells_column_spanners(spanners = c("Cumulative metrics"))
      ) %>%
      # adjust title font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(24))),
        locations = list(cells_title(groups = "title"))
      ) %>%
      # adjust subtitle font
      tab_style(
        style     = list(cell_text(font = "helvetica", size = px(18))),
        locations = list(cells_title(groups = "subtitle"))
      ) %>%
      # highlight national estimate
      tab_style(
        style = cell_fill(color = "#fcf8d4"),
        locations = cells_body(rows = Location == "India")
      )

    cli::cli_alert_success("top 20 cumulative table made")

  }

  if (is.null(top20)) {
    return(list(full       = tabl,
         point_in_time     = point_in_time,
         cumulative        = cumulative
    ))
  } else {
    return(list(full       = tabl,
         point_in_time     = point_in_time,
         cumulative        = cumulative,
         full_t20          = t20_tabl,
         point_in_time_t20 = t20_point_in_time,
         cumulative_t20    = t20_cumulative
    ))
  }

}

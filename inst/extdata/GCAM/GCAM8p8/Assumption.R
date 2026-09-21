# This script is provided as a convenience tool for generating exogenous parameters that are stored in inst/extdata/ for use by the package.
# Package functions do not automatically run this script. If users wish to modify any of the underlying assumptions
# (for example, to align with a different GCAM version or a customized GCAM configuration),
# they must manually run this script to regenerate the corresponding inputs.

# This script reconstructs the retirement/S-curve (SCurve) and O&M cost
# (OM_cost) assumption objects from GCAM-USA 8.8 input tables. This package
# version targets GCAM-USA 8.8 exclusively, so after running this script,
# save the result over the package's SCurve/OM_cost data directly:
#   usethis::use_data(SCurve, OM_cost, overwrite = TRUE)
#

# MODEL_FUTURE_YEARS and the state list are defined locally below rather
# than pulled from gcamdata

read.GCAM.csv <- function(basename, na.strings = "") {
  pathname <- file.path('inst/extdata/GCAM/GCAM8p8', paste0(basename, ".csv"))
  return(read.csv(pathname, na.strings = na.strings, stringsAsFactors = FALSE, comment.char = "#"))
}

# GCAM-USA 8.8 period structure and states ----
# 2021 is a historical/calibration year (not a projected future period) -
# the final-historical-year moved from 2015 (v7.1) to 2021 (v8.8), so the
# first true future year is 2025, not 2021.
MODEL_FUTURE_YEARS_8P8 <- seq(2025, 2100, by = 5)

GCAMUSA_STATES_8P8 <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
                        "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
                        "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
                        "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA",
                        "WI", "WV", "WY")

# GCAM-USA 8.8 s-curve assumption ----

A23.globaltech_retirement <- read.GCAM.csv("A23.globaltech_retirement")
A23.elecS_tech_mapping_cool <- read.GCAM.csv("A23.elecS_tech_mapping_cool")

L2233.StubTechSCurve_elecS_cool_USA <- read.GCAM.csv("L2233.StubTechSCurve_elecS_cool_USA") %>%
  tidyr::as_tibble()

L2233.GlobalTechSCurve_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalTechSCurve_elecS_cool_USA") %>%
  dplyr::filter(subsector.name != "battery") %>%
  tidyr::as_tibble()

L2233.GlobalTechSCurve_elecS_cool_USA %>%
  gcamdata::repeat_add_columns(tibble::tibble(region = GCAMUSA_STATES_8P8)) %>%
  dplyr::rename(subsector0 = subsector.name0, subsector = subsector.name) %>%
  dplyr::full_join(L2233.StubTechSCurve_elecS_cool_USA,
            by = c("region", "supplysector", "subsector0", "subsector", "year", "technology")) %>%
  # use info from L2233.StubTechSCurve_elecS_cool_USA when available
  dplyr::mutate(lifetime = ifelse(is.na(lifetime.y), lifetime.x, lifetime.y),
                steepness = ifelse(is.na(steepness.y), steepness.x, steepness.y),
                half.life = ifelse(is.na(half.life.y), half.life.x, half.life.y)) %>%
  dplyr::select(-lifetime.x, -steepness.x, -half.life.x, -lifetime.y, -steepness.y, -half.life.y,
                -supplysector, -subsector0) ->
  SCurve_baseyear

# for non-base year vintage, natural retirement happens when reaching lifetime
# and assign NA for steepness and half.life for future calculation

SCurve_future <- tibble::as_tibble(A23.globaltech_retirement) %>%
  dplyr::mutate(year = dplyr::case_when(year == "final-historical-year" ~ "final-calibration-year",
                                        year == "initial-nonhistorical-year" ~ "initial-future-year",
                                        T ~ year)) %>%
  dplyr::filter(grepl("initial", year)) %>%
  dplyr::left_join(A23.elecS_tech_mapping_cool, by = c("supplysector", "subsector", "technology")) %>%
  dplyr::select(subsector = Electric.sector.technology,
                technology = to.technology,
                lifetime, half.life, steepness) %>%
  gcamdata::repeat_add_columns(tibble::tibble(region = GCAMUSA_STATES_8P8)) %>%
  gcamdata::repeat_add_columns(tibble::tibble(year = MODEL_FUTURE_YEARS_8P8))


SCurve_baseyear %>%
  dplyr::bind_rows(SCurve_future) %>%
  dplyr::rename(vintage = year) ->
  SCurve

# GCAM-USA 8.8 O&M assumption ----

# Electricity Load Segments Technology Fixed OM Costs (1975$/kW/yr)
L2233.GlobalTechOMfixed_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalTechOMfixed_elecS_cool_USA")
L2233.GlobalIntTechOMfixed_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalIntTechOMfixed_elecS_cool_USA") %>%
  dplyr::rename(sector.name = supplysector, subsector.name0 = subsector0, subsector.name = subsector)

# Electricity Load Segments Technology Variable OM Costs (1975$/MWh)
L2233.GlobalTechOMvar_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalTechOMvar_elecS_cool_USA")
L2233.GlobalIntTechOMvar_elecS_cool_USA <- read.GCAM.csv("L2233.GlobalIntTechOMvar_elecS_cool_USA") %>%
  dplyr::rename(sector.name = supplysector, subsector.name0 = subsector0, subsector.name = subsector)

L2233.GlobalTechOMvar_elecS_cool_USA %>%
  bind_rows(L2233.GlobalIntTechOMvar_elecS_cool_USA %>% rename(technology = intermittent.technology)) %>%
  dplyr::mutate(unit = "1975$/MWh",
                technology = gsub(" ", "", technology)) %>%
  dplyr::rename(value = OM.var,
                input = input.OM.var) ->
  OM_var


L2233.GlobalTechOMfixed_elecS_cool_USA %>%
  bind_rows(L2233.GlobalIntTechOMfixed_elecS_cool_USA %>% rename(technology = intermittent.technology)) %>%
  dplyr::mutate(unit = "1975$/kW/yr",
                technology = gsub(" ", "", technology)) %>%
  dplyr::rename(value = OM.fixed,
                input = input.OM.fixed) ->
  OM_fix

# CSP technology OM_var info does not have further technology information
# include more detailed technology-level info , in later versions of GCAM, this might change when the assumption is better specified

OM_fix %>%
  dplyr::bind_rows(OM_var) %>%
  dplyr::select(-unit) %>% spread(input, value) %>%
  dplyr::group_by(sector.name, subsector.name0, subsector.name, year) %>%
  dplyr::mutate(`OM-fixed` = ifelse(is.na(`OM-fixed`), mean(`OM-fixed`, na.rm = TRUE), `OM-fixed`),
                `OM-var` = ifelse(is.na(`OM-var`), mean(`OM-var`, na.rm = TRUE), `OM-var`)) ->
  OM_cost

# After running this script:
#   usethis::use_data(SCurve, OM_cost, overwrite = TRUE)

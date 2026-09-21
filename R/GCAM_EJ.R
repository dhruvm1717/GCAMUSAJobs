
#' GCAM_EJ
#'
#' @param prj A list of queried outcome
#' This function takes +20 seconds for a single scenario
#' @import dplyr tidyr rgcam tibble
#' @return A data frame with state-level power output from capacity activity
#' @export
#'
#' @examples
#' \dontrun{
#' EJ_activity <- GCAM_EJ(prj)
#' }


GCAM_EJ <- function(prj){

  elec_input <- getQuery(prj, 'elec energy input by elec gen tech by vintage') %>%
     select(scenario, region, subsector = subsector...5, subsector.1 = subsector...6,
           technology, input, Year = year, value, Units) %>%
     mutate(subsector = gsub(",depth=1", "", subsector.1)) %>%
     select(-subsector.1) %>%
     separate(technology, c("technology", "temp"), sep = ",") %>%
     separate(temp, c("temp", "vintage"), sep = "=") %>%
     select(-temp) %>%
     mutate(vintage = as.numeric(vintage)) %>%
     filter(vintage >= min(Year),
                  Year >= vintage)

  ## conventional ----
  elec_vintage0 <- getQuery(prj, 'elec gen by gen tech and cooling tech (incl cogen) by vintage') %>%
     select(scenario, region, subsector = subsector...5, subsector.1 = subsector...6,
           technology, Year = year, value, Units) %>%
     mutate(subsector = gsub(",depth=1", "", subsector.1)) %>%
     group_by(scenario, region, subsector, technology, Year, Units) %>%
     summarise(value = sum(value), .groups = "drop") %>%
     separate(technology, c("technology", "temp"), sep = ",") %>%
     separate(temp, c("temp", "vintage"), sep = "=") %>%
     select(-temp) %>%
     mutate(vintage = as.numeric(vintage)) %>%
     filter(vintage >= min(Year),
                  Year >= vintage)

  YEAR_RANGE <- sort(unique(elec_vintage0$Year))

  # find scenario's last model year
  elec_vintage0 %>%
     group_by(scenario) %>%
     summarise(scenario_max_year = max(Year), .groups = "drop") ->
    scenario_max_year

  ## renewable resource ----
  # annual, by subsector, technology and vintage, EJ

  ### elec_gen & vintage calculation for rooftop_PV ----

  # use the input to query correct rooftop PV elec_gen output
  elec_input %>%
     filter(grepl("rooftop", subsector)) %>%
     filter(input =="distributed_solar") %>%
     group_by(scenario, region, subsector, technology, input, Year, Units) %>%
     summarise(value = sum(value), .groups = "drop") ->
    elec_input_RTPV

  YEAR_RANGE_RTPV <- sort(unique(elec_input_RTPV$Year))

  # starting Year
  A <- min(YEAR_RANGE_RTPV)
  # set lifetime of rooftop = 20 years
  RTPVLT <- 20

  elec_input_RTPV %>%
     group_by(scenario, region, subsector, technology) %>%
     mutate(!!paste0("Vin", A) := value[Year == A],
                  !!paste0("Vin", A) := ifelse(Year >= A & Year < A + RTPVLT,
                                                .data[[paste0("Vin", A)]][Year == A], 0 )) %>%
     ungroup() ->
    df

  for (i in 2:length(YEAR_RANGE_RTPV)) {
    TIME <- YEAR_RANGE_RTPV[i]
    df %>%
       mutate(!!paste0("Vin", TIME) := ifelse(value - rowSums( across( starts_with("Vin"))) > 0, # only add new vintage capacity if older vintage is not enough
                                                    value - rowSums( across( starts_with("Vin"))),
                                                    0)) -> # if previous vintage is enough, then 0 new vintage
      df
    df %>%
       group_by(scenario, region, subsector, technology) %>%
       mutate(!!paste0("Vin", TIME) := ifelse(Year >= TIME & Year < TIME + RTPVLT, .data[[paste0("Vin", TIME)]][Year == TIME], 0)) ->
      df
  }
  df %>%
     rename(running = value) %>%
     gather(vintage, value, c(starts_with("Vin"))) %>%
     mutate(vintage = gsub("Vin", "", vintage),
                  vintage = as.numeric(vintage)) %>%
     select(names(elec_vintage0)) %>%
     filter(Year >= vintage) ->
    elec_gen_RTPV_vintage

  ### elec_gen calculation for PV and CSP ----
  elec_input %>%
     filter(grepl("PV", subsector)|grepl("CSP", subsector)) %>%
     filter(input !="backup_electricity") %>%
     group_by(scenario, region, subsector, technology, vintage, Year, Units) %>%
     summarise(value = sum(value), .groups = "drop") %>%
     select(names(elec_vintage0)) %>%
     filter(Year >= vintage) ->
    elec_gen_SOLAR_vintage

  ### elec_gen calculation for wind ----
  elec_input %>%
     filter(grepl("wind", subsector)) %>%
     filter(input !="backup_electricity") %>%  group_by(scenario, region, subsector, technology, vintage, Year, Units) %>%
     summarise(value = sum(value), .groups = "drop") %>%
     select(names(elec_vintage0)) %>%
     filter(Year >= vintage) ->
    elec_gen_WIND_vintage


  # combine elec_gen by vintage across fuel types  ----
  # solar and wind are calculated separately for the backup_elec adjustment
  # rooftop is calculated separately for the backup_elec adjustment and vintage structure

  elec_vintage0 %>%
     filter(!grepl("CSP", subsector)) %>% # solar calculated separately
     filter(!grepl("PV", subsector)) %>% # solar calculated separately
     filter(!grepl("rooftop", subsector)) %>% # rooftop pv calculated and vintaged separately
     filter(!grepl("wind", subsector)) %>% # wind calculated separately
     bind_rows(elec_gen_RTPV_vintage) %>% # add rooftop PV elec_vintage
     bind_rows(elec_gen_SOLAR_vintage) %>% # add rooftop CSP and PV elec_vintage
     bind_rows(elec_gen_WIND_vintage) -> # add rooftop wind elec_vintage
    elec_vintage
  ##  elec_gen from installed capacity ----

  elec_vintage %>%
     filter(Year >= vintage) %>%
     arrange(scenario, region, subsector, technology, vintage, desc(Year)) %>%
     group_by(scenario, region, subsector, technology, vintage) %>%
     mutate(real_cap = cummax(value)) %>% # use max of future operating capacity as the real capacity
     arrange(scenario, region, subsector, technology, vintage, Year) %>%
     ungroup() ->
    elec_vintage_real

  # Calculate s-curve output fraction ----
  # Expand to full YEAR_RANGE before evaluating the formula

  elec_vintage_real %>%
     distinct(scenario, region, subsector, technology, vintage, Units) %>%
    repeat_add_columns(tibble(Year = YEAR_RANGE)) %>%
     filter(Year >= vintage) %>%
     left_join(SCurve,
              by = c("region", "subsector", "technology", "vintage")) %>%
     mutate(half.life = as.numeric(half.life),
           steepness = as.numeric(steepness),
           half.life = ifelse(is.na(half.life), 0, half.life),
           steepness = ifelse(is.na(steepness), 0, steepness),
           s_curve_frac = ifelse(Year > vintage & half.life != 0,
                                  (1 / (1 + exp( steepness * ((Year - vintage) - half.life )))),
                                  1)) %>%
     distinct() ->
    s_curve_frac


  # Adjust s-curve output fraction to ensure that all of the capacity is retired at the end of lifetime
  s_curve_frac %>%
     mutate(s_curve_adj =  if_else(Year - vintage >= lifetime, 0, s_curve_frac),
                  s_curve_adj =  if_else(is.na(s_curve_adj), 1, s_curve_adj)) %>%
     select(scenario, region, subsector, technology, vintage, Units, Year, s_curve_adj) ->
    s_curve_frac_adj

  # no further expansion needed.
  s_curve_frac_adj %>%
    # assume hydro does not retire
     mutate(s_curve_adj = ifelse(grepl("hydro", subsector), 1, s_curve_adj),
           # assume rooftop_PV has a 20 years lifetime, so s-curve-frac = 0 in year vintage + 20
           s_curve_adj = ifelse(grepl("rooftop", subsector), 0, s_curve_adj),
           s_curve_adj = ifelse(grepl("rooftop", subsector) & (Year < vintage + 20), 1, s_curve_adj)) %>%
     filter(Year >= vintage) ->
    s_curve_frac_adj_full

  ### elec_gen from newly installed capacity ----
  elec_vintage_real %>%
     filter(!grepl("geo", subsector)) %>%
     mutate(additions =  if_else(vintage == Year, value, 0),
                  additions =  if_else(grepl("hydro", subsector), 0, additions)) ->
    elec_vintage_add_nogeo

  elec_vintage_add_nogeo %>%
     group_by(scenario, region, subsector, technology, Units, Year) %>%
     summarise(additions = sum(additions), .groups = "drop") ->
    elec_total_add_nogeo

  # Year-over-year change in total installed capacity

  elec_vintage_real %>%
     group_by(scenario, region, subsector, Units, Year) %>%
     summarise(total_installed = sum(real_cap), .groups = "drop") %>%
     arrange(scenario, region, subsector, Units, Year) %>%
     group_by(scenario, region, subsector, Units) %>%
     mutate(delta_installed = total_installed - lag(total_installed)) %>%
     ungroup() %>%
     select(scenario, region, subsector, Units, Year, delta_installed) ->
    total_installed_delta

  ### elec_gen from retired capacity ----

  s_curve_frac_adj_full %>%
     left_join(elec_vintage_real %>%
                        filter(!grepl("geo", subsector)) %>% # calculate additions and retirement of geothermal separately
                        filter(vintage == Year) %>%
                        select(-Year, -value) %>%
                        rename(OG_gen = real_cap),
                     by = c("scenario", "region", "subsector", "technology", "vintage", "Units")) %>%
     mutate(gen_expect = OG_gen * s_curve_adj) ->
    elec_gen_expect

  elec_gen_expect %>%
     arrange(scenario, region, subsector, technology, vintage, Units, Year) %>%
     group_by(scenario, region, subsector, technology, Units, vintage) %>%
     mutate(prev_yr_expect =  lag(gen_expect, n = 1L),
                  natural_retire =  if_else(Year > vintage & prev_yr_expect > gen_expect,
                                                  prev_yr_expect - gen_expect, 0)) %>%
     ungroup() %>%
     select(-s_curve_adj, -OG_gen, -gen_expect, -prev_yr_expect) ->
    elec_retire_expect

  elec_vintage_real %>%
     filter(!grepl("geo", subsector)) %>% # calculate additions and retirement of geothermal separately
     select(-Year, -value, -real_cap) %>%  distinct() %>%
    repeat_add_columns(tibble(Year = YEAR_RANGE)) %>%  # expand the data to include years w/o production
     left_join(elec_vintage_real,
                     by = c("scenario", "region", "subsector", "technology", "vintage", "Units", "Year")) %>%
     replace_na(list(real_cap = 0)) %>%  # NA means 0 production from a vintage in this year
     filter(Year >= vintage) %>%
     arrange(scenario, region, subsector, technology, vintage, Units, Year) %>%
     group_by(scenario, region, subsector, technology, Units, vintage) %>%
     mutate(prev_year =  lag(real_cap, n = 1L)) %>%
     mutate(retirements = ifelse(is.na(prev_year), 0, prev_year - real_cap),
                  retirements = ifelse(retirements < 0, 0, retirements),
                  retirements = ifelse(grepl("hydro", subsector), 0, retirements)) %>%
     arrange(vintage, technology, region) %>%
     ungroup() ->
    elec_vintage_ret

  elec_vintage_ret %>%
     select(-value, - real_cap, -prev_year) %>%
    left_join_error_no_match(elec_retire_expect,
                             by = c("scenario", "region", "subsector", "technology", "vintage", "Units", "Year")) %>%
     left_join(scenario_max_year, by = "scenario") %>%
    # nuclear/rooftop: use S-curve expected retirement
     mutate(retirements = case_when(
              (grepl("nuc", subsector) | grepl("rooftop", subsector)) & Year <= scenario_max_year ~ natural_retire,
              Year > scenario_max_year ~ 0,
              TRUE ~ retirements)) %>%
     select(-scenario_max_year) %>%
    # when there is early retirement in previous periods, the expected natural retirement when reaching lifetime can be larger than the observed retirement
    # therefore, adjust the natural retirement of each period, cap by the observed retirement
     mutate(natural_retire = ifelse(natural_retire > retirements, retirements, natural_retire),
           early_retire = retirements - natural_retire) ->
    elec_retire_all_nogeo

  # Total retirements per region/ technology/ year (in EJ)
  elec_retire_all_nogeo %>%
     group_by(scenario, region, subsector, technology, Units, Year) %>%
     summarise(retirements = sum(retirements),
                     natural_retire = sum(natural_retire),
                     early_retire = sum(early_retire),
                     .groups = "drop") ->
    elec_total_ret_nogeo

  # Adjusted (Net) additions and retirements
  # The key question here is do we allow capacity addition and retirement to happen in the same year for a technology in a region
  # Natural retirement is not affected by this adjustment
  # If not allowed:
  # when early retirement is larger than addition, then net retire
  # when early retirement is smaller than addition, then net add
  # Merge total additions and retirements data tables
  elec_total_ret_nogeo %>%
     left_join(elec_total_add_nogeo, by = c("scenario", "region", "subsector", "technology", "Units", "Year")) %>%
     replace_na(list(additions = 0)) ->
    elec_add_ret_merged

  # derive nuclear additions re-derived from  installed-capacity

  elec_add_ret_merged %>%
     filter(grepl("nuc", subsector)) %>%
     group_by(scenario, region, subsector, Units, Year) %>%
     summarise(retirements = sum(retirements),
                     natural_retire = sum(natural_retire),
                     early_retire = sum(early_retire),
                     additions = sum(additions),
                     .groups = "drop") %>%
     left_join(total_installed_delta, by = c("scenario", "region", "subsector", "Units", "Year")) %>%
     mutate(additions = ifelse(!is.na(delta_installed), pmax(0, delta_installed + retirements), additions)) %>%
     select(-delta_installed) %>%
     mutate(technology = subsector) -> # nuclear no longer has technology breakdown
    elec_add_ret_nuc

  elec_add_ret_merged %>%
     filter(!grepl("nuc", subsector)) ->
    elec_add_ret_nonnuc

  # subtract real 2024 historic generation from rpv first vintage year value:
  # TEMPORARY MEASURE BECAUSE OF FIXED OUTPUT BEING APPLIED THROUGH ADDON FILE

  elec_add_ret_nonnuc %>%
     left_join(rooftop_hist_gen %>% select(region, hist_gen = value), by = "region") %>%
     mutate(additions = ifelse(grepl("rooftop", subsector) & Year == A,
                               pmax(0, additions - coalesce(hist_gen, 0)),
                               additions)) %>%
     select(-hist_gen) ->
    elec_add_ret_nonnuc

  elec_add_ret_nonnuc %>%
     bind_rows(elec_add_ret_nuc) %>%
     mutate(add_adj =  if_else(additions >= early_retire, additions - early_retire, 0),
                  early_ret_adj =  if_else(early_retire > additions, early_retire - additions, 0),
                  ret_adj = early_ret_adj + natural_retire) -> # add the natural retire to get the updated total ret
    elec_add_ret_nogeo

  ## elec_gen activities of geothermal ----

  # Geothermal: built once at GCAM_HIST_YEAR, rebuilt every GEOLT years,
  # target years snapped to the nearest real future year. See NOTES.md "Geothermal".
  GEOLT <- 30
  GEO_HIST_YEAR <- GCAM_HIST_YEAR
  GEO_FUTURE_YEARS <- gcam_future_years(YEAR_RANGE)
  # guard against seq() erroring when the horizon doesn't reach one geothermal lifetime past GEO_HIST_YEAR
  if (max(YEAR_RANGE) >= GEO_HIST_YEAR + GEOLT) {
    GEO_REBUILD_TARGETS <- seq(GEO_HIST_YEAR + GEOLT, max(YEAR_RANGE), by = GEOLT)
    GEO_REBUILD_YEARS <- unique(sapply(GEO_REBUILD_TARGETS, function(target) {
      GEO_FUTURE_YEARS[which.min(abs(GEO_FUTURE_YEARS - target))]
    }))
  } else {
    GEO_REBUILD_YEARS <- numeric(0)
  }

  elec_vintage %>%
     filter(Year >= vintage) %>%
     filter(grepl("geo", subsector)) %>%
     arrange(scenario, region, subsector, technology, vintage, desc(Year)) %>%
     group_by(scenario, region, subsector, technology) %>%
     mutate(real_cap = cummax(value)) %>% # use max of future operating capacity as the real capacity
     arrange(scenario, region, subsector, technology, Year) %>%
     mutate(additions = ifelse(Year %in% c(GEO_HIST_YEAR, GEO_REBUILD_YEARS), real_cap, 0),
                  retirements = ifelse(Year %in% GEO_REBUILD_YEARS, real_cap, 0),
                  natural_retire = ifelse(Year %in% GEO_REBUILD_YEARS, real_cap, 0),
                  early_retire = 0,
                  add_adj = additions,
                  early_ret_adj = early_retire,
                  ret_adj = retirements) %>%
     select(names(elec_add_ret_nogeo)) ->
    elec_add_ret_geo

  elec_add_ret_nogeo %>%
     bind_rows(elec_add_ret_geo) ->
    elec_add_ret

  elec_vintage %>%
     group_by(scenario, region, subsector, technology, Year, Units) %>%
     summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
     mutate(activity = "running") %>%
     bind_rows(elec_vintage_real %>%
                        group_by(scenario, region, subsector, technology, Year, Units) %>%
                        summarise(value = sum(real_cap, na.rm = T), .groups = "drop") %>%
                        mutate(activity = "installed")) %>%
     bind_rows(elec_add_ret %>%  gather(activity, value, retirements:ret_adj)) ->
    elec_activity


  cap_fac_join <- rgcam::getQuery(prj, 'GCAM_USA elec cap-fac by cooling tech') %>%
    select(scenario, region, year, sector, subsector = subsector...5,
           subsector.1 = subsector...6, technology, value) %>%
    mutate(fuel = subsector,
           fuel = ifelse(grepl("offshore", subsector.1), "wind_offshore", fuel),
           subsector = gsub(",depth=1", "", subsector.1),
           technology = gsub(" ", "", technology),
           fuel = ifelse(grepl("CSP", subsector), "CSP", fuel), # separate CSP and PV from solar
           fuel = ifelse(grepl("PV", subsector), "PV", fuel))


  elec_activity %>%
    filter(!grepl("hydro", subsector)) %>%
    filter(!grepl("geo", subsector)) %>%
    filter(!grepl("nuc", subsector)) %>%
    mutate(technology = gsub(" ", "", technology)) %>%
    left_join(cap_fac_join %>% rename(Year = year, capacity.factor = value),
            by = c("scenario", "region", "subsector", "technology", "Year")) %>%
    bind_rows(elec_activity %>%
                filter(grepl("geo", subsector)) %>%
                mutate(technology = gsub(" ", "", technology)) %>%
                mutate(fuel = ifelse(grepl("geo", subsector), "geo", fuel),
                       capacity.factor = geo_cf)) %>%
    bind_rows(elec_activity %>%
                filter(grepl("nuc", subsector)) %>%
                mutate(technology = gsub(" ", "", technology)) %>%
                mutate(fuel = ifelse(grepl("nuc", subsector), "nuclear", fuel),
                       capacity.factor = nuc_cf)) %>%
    bind_rows(elec_activity %>%
                filter(grepl("hydro", subsector)) %>%
                mutate(technology = gsub(" ", "", technology)) %>%
                left_join(hydro_cf %>%  rename(capacity.factor = value), by = "region")) %>%
    select(-sector, -subsector.1)->
    elec_gen_activity

  EJ_activity <- list(
    elec_gen_activity = elec_gen_activity,
    cap_fac_join = cap_fac_join
  )

  # OUTPUT : elec_activity ----
  return(EJ_activity)
}

rm(list = ls())

# load parlgov database -------------------------------------------------------

# all parties 
sqlite_driver <- RSQLite::dbDriver("SQLite")
parlgov <- RSQLite::dbConnect(sqlite_driver,
                              dbname = "~/papers/pledge_paper/data/parlgov.db")

RSQLite::dbListTables(parlgov)

parlgov_elections <- RSQLite::dbReadTable(parlgov, "view_election")

# prepare marpor data ----------------------------------------------------------
# https://doi.org/10.1111/j.1939-9162.2010.00006.x
# Lowe 2011 log scaling 
immigration_position <- data.table::fread("~/papers/pledge_paper/data/marpor_2024.csv") |>
  dplyr::mutate(
    marpor_party_id = party,
    election_date = as.Date(edate, format = "%d/%m/%Y"),
    anti_immigration_position = log((per608 + 0.5) / (per607 + 0.5))
  ) |>
  dplyr::select(countryname, partyname, marpor_party_id, election_date, anti_immigration_position)


marpor_parlgov_mapping <- RSQLite::dbReadTable(parlgov, "party") |>
  dplyr::select(id, country_id, cmp, name_english) |>
  dplyr::left_join(parlgov_elections |> dplyr::distinct(country_name, country_id), "country_id") |>
  dplyr::select(country_name, country_id, name_english, id, cmp) |>
  magrittr::set_colnames(c("countryname", "country_id", "partyname", "party_id", "marpor_party_id"))

# parglov is not 100% up to date with marpor party IDs
# we thus fuzzy match on country and party to retrieve marpor IDs that may 
# not be up to date in parlgov
marpor_parglov_mapping_na <- marpor_parlgov_mapping |>
  dplyr::filter(is.na(marpor_party_id)) |>
  dplyr::filter(!partyname %in% c("no seat", "one seat", "others", "no party affiliation")) |>
  tidyr::unite(merge_key, c("countryname","partyname"), remove = FALSE) |>
  dplyr::select(merge_key, countryname, country_id, partyname, party_id)

immigration_position_unique <- immigration_position |> 
  dplyr::distinct(countryname, partyname, marpor_party_id) |> 
  dplyr::distinct(marpor_party_id, .keep_all = TRUE) |>
  tidyr::unite(merge_key, c("countryname","partyname"), remove = FALSE )

marpor_parglov_mapping_completion <- fedmatch::merge_plus(
  data1 = marpor_parglov_mapping_na, 
  data2 = immigration_position_unique,
  match_type = "fuzzy",
  by = "merge_key",
  unique_key_1 = "party_id",
  unique_key_2 = "marpor_party_id",
  fuzzy_settings = fedmatch::build_fuzzy_settings(method = "jw", maxDist = 0.05)
)

marpor_parglov_mapping_completion <- marpor_parglov_mapping_completion$matches |>
  dplyr::select(countryname_1, country_id, partyname_1, party_id, marpor_party_id) |>
  magrittr::set_colnames(c("countryname", "country_id", "partyname", "party_id", "marpor_party_id"))

marpor_parlgov_mapping <- marpor_parlgov_mapping |>
  dplyr::filter(!
    (countryname %in% marpor_parglov_mapping_completion$countryname &
    country_id %in% marpor_parglov_mapping_completion$country_id &
    partyname %in% marpor_parglov_mapping_completion$partyname &
    party_id %in% marpor_parglov_mapping_completion$party_id)
  ) |>
  rbind(marpor_parglov_mapping_completion)

rm(marpor_parglov_mapping_na, immigration_position_unique, marpor_parglov_mapping_completion)

# join parlgov data to immigration positions
immigration_position <- immigration_position |>
  dplyr::left_join(
    marpor_parlgov_mapping |>
      dplyr::select(country_id, party_id, marpor_party_id), 
    by = "marpor_party_id"
  )

# prepare data on avg government positions based on marpor ---------------------
# unique cabinets
cabinets <- RSQLite::dbReadTable(parlgov, "view_cabinet")

multi_cabinet_elections <- cabinets |> 
  dplyr::group_by(country_name, election_date, election_id) |> 
  dplyr::summarise(n_cabinets = length(unique(cabinet_id))) |>
  dplyr::filter(n_cabinets > 1)

multi_cabinet_elections <- cabinets |>
  dplyr::filter(election_id %in% multi_cabinet_elections$election_id) |>
  dplyr::filter(cabinet_party == 1) |>
  dplyr::group_by(country_name_short, election_id, cabinet_id) |>
  dplyr::summarise(
    party_id = list(unique(party_id)),
    previous_cabinet_id = unique(previous_cabinet_id)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(country_name_short, election_id) |>
  dplyr::group_split()

cabinets_to_drop <- lapply(multi_cabinet_elections, function(election) {
  election$party_change <- 1
  for(i in 1:nrow(election)) {
    prev_cabinet_id <- election[i,"previous_cabinet_id"] 
    
    if(prev_cabinet_id %in% election$cabinet_id) {
      current_cabinet_comp <- unlist(election[i, "party_id"])
      prev_cabinet_comp <- unlist(election[election$cabinet_id == prev_cabinet_id, "party_id"])
      
      if(setequal(current_cabinet_comp, prev_cabinet_comp)) {
        election[i,"party_change"] <- 0
      }
    } else {
      next
    }
  }
  return(election)
}) |>
  dplyr::bind_rows() |>
  dplyr::filter(party_change == 0) |>
  dplyr::select(country_name_short, election_id, cabinet_id)

cabinets <- cabinets |>
  dplyr::filter(!(country_name_short %in% cabinets_to_drop$country_name_short & 
                  election_id %in% cabinets_to_drop$election_id &
                  cabinet_id %in% cabinets_to_drop$cabinet_id))


# add immigration position to party data 
party_immigraiton_positions <- cabinets |>
  dplyr::mutate(election_date = as.Date(election_date)) |>
  dplyr::left_join(immigration_position, by = c("party_id", "country_id", "election_date"))

rm(immigration_position)

# add right-wing party family membership 
rightwing_parties <- RSQLite::dbReadTable(parlgov, "party") |>
  dplyr::mutate(right_wing = ifelse(family_id == 40, 1, 0)) |>
  dplyr::select(country_id, id, right_wing) |>
  magrittr::set_colnames(c("country_id", "party_id", "right_wing"))

# join immigration position & right wing data 
party_immigraiton_positions <- party_immigraiton_positions |>
  dplyr::left_join(rightwing_parties, by = c("country_id", "party_id"))

rrp_immigration_positions <- party_immigraiton_positions |>
  dplyr::filter(right_wing == 1) |>
  dplyr::select(country_id, election_id, cabinet_id, party_id, anti_immigration_position) |>
  dplyr::rename(right_wing_anti_immigration_position = anti_immigration_position)

# opposition without right party left-right + anti-immigration
opposition_immigration_positions <- party_immigraiton_positions |>
  dplyr::filter(cabinet_party == 0) |>
  dplyr::filter(right_wing != 1) |>
  dplyr::mutate(seat_weight = seats / election_seats_total) |>
  dplyr::group_by(country_id, election_id, cabinet_id) |>
  dplyr::mutate(seat_weight = seat_weight / sum(seat_weight, na.rm = TRUE)) |>
  dplyr::summarise(
    opposition_left_right = mean(left_right * seat_weight),
    opposition_anti_immigration_position = mean(anti_immigration_position * seat_weight)
  ) |>
  dplyr::ungroup()

# aggregate government positions
government_immigration_positions <- party_immigraiton_positions |>
  dplyr::filter(cabinet_party == 1) |>
  dplyr::mutate(seat_weight = seats / election_seats_total) |>
  dplyr::group_by(
    country_name_short, country_name, election_date, start_date, country_id,
    election_id, cabinet_id, previous_cabinet_id
  ) |>
  dplyr::mutate(seat_weight = seat_weight / sum(seat_weight, na.rm = TRUE)) |>
  dplyr::summarise(
    left_right = mean(left_right * seat_weight),
    anti_immigration_position = mean(anti_immigration_position * seat_weight),
    right_wing_in_gov = ifelse(sum(right_wing) > 0, 1, 0)
  ) |>
  dplyr::ungroup() 

# construct a country, year, government panel
cabinet_ids <- government_immigration_positions$cabinet_id
cabinet_ids <- cabinet_ids[!is.na(cabinet_ids)]

panel_data <- lapply(cabinet_ids, function(id) {
  tryCatch({
    cabinet_i <- government_immigration_positions |>
      dplyr::filter(cabinet_id == id)
    
    cabinet_prev <- government_immigration_positions |>
      dplyr::filter(cabinet_id == unique(cabinet_i$previous_cabinet_id))
    
    cabinet_years <- lubridate::year(cabinet_prev$start_date):lubridate::year(cabinet_i$start_date)
    
    expand.grid(
      list(
        "cabinet_id" = unique(cabinet_prev$cabinet_id),
        "year" = cabinet_years,
        "end_date" = unique(cabinet_i$start_date)
      )
    )
  }, error = function(e) {
    NULL  
  })
})

panel_data <- Filter(Negate(is.null), panel_data) |>
  dplyr::bind_rows()

panel_data <- panel_data |> 
  dplyr::left_join(government_immigration_positions, by = c("cabinet_id")) |>
  dplyr::select(cabinet_id, year, country_name_short, country_name, election_date,
                start_date, end_date, country_id, election_id, previous_cabinet_id,
                left_right, anti_immigration_position, right_wing_in_gov) |>
  dplyr::mutate(
    start_date = as.Date(as.character(start_date)),
    end_date = as.Date(as.character(end_date))
  )


# add outcome data to panel ----------------------------------------------------

# if a given cabinet only covers part of a full year (i.e. there is an election
# at some point during a given year) we need to weight the year level outcome
# by the fraction of the year for which a given cabinet governed 
get_partial_year_weight <- function(year, start_date, end_date) {
  mapply(function(y, s, e) {
    # define the year's boundaries
    year_start <- as.Date(paste0(y, "-01-01"))
    year_end <- as.Date(paste0(y, "-12-31"))
    
    # find the overlap period
    overlap_start <- max(s, year_start)
    overlap_end <- min(e, year_end)
    
    # calculate days contained
    days_contained <- as.numeric(overlap_end - overlap_start + 1)
    
    # if no overlap, return 0
    if (days_contained <= 0) {
      return(0)
    } 
    
    # check if it's a leap year
    days_in_year <- ifelse(lubridate::leap_year(y), 366, 365)
    
    # return fraction
    return(min(days_contained / days_in_year, 1))
  }, year, start_date, end_date)
}

panel_data$partial_year_weight <- get_partial_year_weight(
  year = panel_data$year,
  start_date = panel_data$start_date,
  end_date = panel_data$end_date
)

# impic data -> 0 == open; 1 == restrictive
impic_outcome <- data.table::fread("~/papers/pledge_paper/data/migration_v4.csv") |>
  dplyr::filter(avgs_immpol >= 0) |>
  dplyr::select(u_complab_country_year_change_country_code, u_complab_country_year_change_year, avgs_immpol) |>
  magrittr::set_colnames(c("country_name_short","year","impic_immigration_restrict")) |>
  dplyr::distinct(country_name_short, year, .keep_all = TRUE)

panel_data <- panel_data |>
  dplyr::left_join(impic_outcome, by = c("country_name_short", "year"))

# add lags of impic outcome --> e.g. lag 1: year 2003 impic gets mapped to year 2004 panel
lags <- 1:4
for (lag in lags) {
  impic_lag <- impic_outcome |>
    dplyr::mutate(year = year + lag) |>
    magrittr::set_colnames(
      c("country_name_short",
        "year",
        paste("impic_immigration_restrict_lag", lag, sep = "")
      )
    )
  
  panel_data <- panel_data |>
    dplyr::left_join(impic_lag, by = c("country_name_short", "year"))
  
}

leads <- 1:4
for (lead in leads) {
  impic_lead <- impic_outcome |>
    dplyr::mutate(year = year - lead) |>
    magrittr::set_colnames(
      c("country_name_short",
        "year",
        paste("impic_immigration_restrict_lead", lead, sep = "")
      )
    )
  
  panel_data <- panel_data |>
    dplyr::left_join(impic_lead, by = c("country_name_short", "year"))
  
}

rm(impic_outcome)



# quantmig data 
quantmig_outcome <- readr::read_tsv("~/papers/pledge_paper/data/quantmig.tsv") |>
  as.data.frame() |>
  tidyr::drop_na(Year) |>
  dplyr::mutate(
    country_name_short = countrycode::countrycode(Country, origin = "country.name", destination = "iso3c"),
    year = Year,
    less_restrictive = ifelse(Restrictiveness %in% c("Less restrictive", "less restrictive"), 1, 0),
    more_restrictive = ifelse(Restrictiveness %in% c("More restrictive", "more restrictive"), 1, 0)
  ) |>
  dplyr::group_by(country_name_short, year) |>
  dplyr::summarise(
    less_restrictive = (sum(less_restrictive) / dplyr::n()) * 100,
    more_restrictive = (sum(more_restrictive) / dplyr::n()) * 100,
    legislation_count = dplyr::n()
  ) |>
  dplyr::ungroup() |>
  tidyr::drop_na() |>
  dplyr::mutate(quantmig_immigration_restrict = log((more_restrictive + 0.5) / (less_restrictive + 0.5))) |>
  dplyr::select(country_name_short, year, quantmig_immigration_restrict, legislation_count)
  
quantmig_panel <- expand.grid(list(country_name_short = unique(quantmig_outcome$country_name_short), year = 1990:2020)) |>
  dplyr::left_join(quantmig_outcome, by = c("country_name_short", "year")) |>
  tidyr::replace_na(list(quantmig_immigration_restrict = 0, legislation_count = 0)) |>
  dplyr::mutate(legislation_weight = legislation_count / sum(legislation_count))


panel_data <- panel_data |> 
  dplyr::left_join(quantmig_panel, by = c("country_name_short", "year"))

rm(quantmig_outcome, quantmig_panel)

# construct treatment ----------------------------------------------------------

# effective (or legal) threshold per election 
# legal thresholds
legal_thrs <- list(
  "New_Zealand" = 5.0,
  "Austria" = 4.0,
  "Bulgaria" = 4.0,
  "Cyprus" = 3.6,
  "Czech_Republic" = 5.0,
  "Estonia" = 5.0,
  "Germany" = 5.0,
  "Greece" = 3.0,
  "Hungary" = 5.0,
  "Italy" = 3.0,
  "Latvia" = 5.0,
  "Lithuania" = 5.0,
  "Netherlands" = 0.67,
  "Poland" = 5.0,
  "Romania" = 5.0,
  "Slovakia" = 5.0,
  "Slovenia" = 4.0,
  "Sweden" = 4.0,
  "Turkey" = 7.0
) |>
  as.data.frame() |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column("country") |>
  magrittr::set_colnames(c("country","legal_thrs")) |>
  dplyr::mutate(country = gsub("_", " ", country)) |>
  dplyr::mutate(country_name_short = countrycode::countrycode(country, origin = "country.name", destination = "iso3c")) |>
  dplyr::select(country_name_short, legal_thrs)

# effective thresholds
electoral_system <- data.table::fread("~/papers/pledge_paper/data/electoral_systems_v4.csv")

eff_thrs <- electoral_system |>
  dplyr::select(country, year, seats, tier1_districts) |>
  magrittr::set_colnames(c("country", "election_year", "s", "d")) |>
  dplyr::mutate(eff_thrs = 75 / ((s/d + 1) * sqrt(d))) |>
  dplyr::mutate(country_name_short = countrycode::countrycode(country, origin = "country.name", destination = "iso3c")) |>
  dplyr::select(country_name_short, election_year, eff_thrs)

# add threshold data
panel_data <- panel_data |>
  dplyr::mutate(election_year = lubridate::year(election_date)) |>
  dplyr::left_join(legal_thrs, by = c("country_name_short")) |>
  dplyr::left_join(eff_thrs, by = c("country_name_short", "election_year"))|>
  dplyr::distinct(cabinet_id, year, country_name_short, .keep_all = TRUE) 

# construct threshold variable
panel_data <- panel_data |>
  dplyr::mutate(thrs = legal_thrs) |>
  dplyr::mutate(thrs = ifelse(is.na(thrs), eff_thrs, thrs))

# add rightwing party 
rightwing_parties <- parlgov_elections |>
  dplyr::select(country_id, election_id, party_id, left_right, vote_share, seats) |>
  dplyr::left_join(rightwing_parties, by = c("country_id", "party_id")) |>
  dplyr::filter(right_wing == 1) |>
  dplyr::mutate(right_wing_in_parl = ifelse(seats > 0, 1, 0)) |>
  dplyr::group_by(country_id, election_id) |>
  dplyr::filter(vote_share == max(vote_share)) |>
  dplyr::ungroup() |>
  dplyr::left_join(rrp_immigration_positions, by = c("country_id", "election_id", "party_id")) |>
  dplyr::distinct(country_id, election_id, party_id, .keep_all = TRUE) |>
  dplyr::select(-c(right_wing, party_id, seats)) |>
  dplyr::rename(right_wing_left_right = left_right,
                right_wing_vote_share = vote_share) 

panel_data <- panel_data |>
  dplyr::left_join(rightwing_parties, by = c("country_id", "election_id")) |>
  dplyr::left_join(opposition_immigration_positions, by = c("country_id", "election_id"))

# center right wing party vote share on threshold
panel_data <- panel_data |>
  dplyr::mutate(right_wing_vote_share_centered = right_wing_vote_share - thrs)

# remove incongruent observations 
panel_data <- panel_data |>
  dplyr::filter(!(right_wing_vote_share_centered > 0 & right_wing_in_parl == 0)) |>
  dplyr::filter(!(right_wing_vote_share_centered < 0 & right_wing_in_parl == 1))

panel_data <- panel_data |>
  dplyr::distinct(country_id,year,election_id,cabinet_id, .keep_all = TRUE)

# case selection ---------------------------------------------------------------
# rightwing party stood for election
panel_data <- panel_data |>
  dplyr::filter(!is.na(right_wing_vote_share))

# threshold of inclusion does not work in majoritarian systems --> exclude from analysis 
leg_type <- electoral_system |>
  dplyr::select(country, year, legislative_type) |>
  dplyr::rename(election_year = year) |>
  dplyr::mutate(majoritarian = ifelse(legislative_type == 1, 1, 0)) |>
  dplyr::mutate(country_name_short = countrycode::countrycode(country, origin = "country.name", destination = "iso3c")) |>
  dplyr::select(country_name_short, election_year, majoritarian)

panel_data <- panel_data |>
  dplyr::left_join(leg_type, by = c("country_name_short", "election_year"))

panel_data <- panel_data |>
  dplyr::distinct(country_id,year,election_id,cabinet_id, .keep_all = TRUE)

# add migration data ----------------------------------------------------------
migration <- data.table::fread("~/papers/pledge_paper/data/net_migration.csv") |>
  janitor::row_to_names(row_number = 1) |>
  dplyr::select(-c(1, 3, 4)) |>
  dplyr::select(-c(V69)) |>
  magrittr::set_colnames(c("country_name_short", as.character(1960:2023))) |>
  tidyr::pivot_longer(!country_name_short, names_to = "year", values_to = "net_migration") |>
  dplyr::group_by(country_name_short) |>
  dplyr::mutate(
    net_migration_lag1 = dplyr::lag(net_migration, 1),
    net_migration_lag2 = dplyr::lag(net_migration, 2)
  ) |>
  dplyr::mutate(
    net_migration_first_dif = net_migration - net_migration_lag1
  ) |>
  dplyr::mutate(year = as.numeric(year))

panel_data <- panel_data |>
  dplyr::left_join(migration, by = c("country_name_short", "year"))

# save data -------------------------------------------------------------------- 
panel_data <- panel_data |>
  dplyr::select(-c(cabinet_id.x, cabinet_id.y)) |>
  dplyr::mutate(gov_rrp_position_diff = left_right - right_wing_left_right) |>
  dplyr::filter(majoritarian != 1) |>
  dplyr::filter(!is.na(quantmig_immigration_restrict)) |>
  dplyr::filter(!is.na(anti_immigration_position)) |> 
  dplyr::mutate(
    net_migration = net_migration / 1e6,
    net_migration_lag1 = net_migration_lag1 / 1e6
  ) |>
  as.data.frame()

panel_data <- panel_data |>
  dplyr::select(
    year, country_name_short, country_name, country_id, cabinet_id, election_date, 
    election_year, start_date, end_date, right_wing_in_gov, right_wing_vote_share,
    right_wing_vote_share_centered, legal_thrs, eff_thrs, thrs, anti_immigration_position, 
    right_wing_anti_immigration_position, opposition_anti_immigration_position, 
    left_right, right_wing_left_right, opposition_left_right, gov_rrp_position_diff, 
    quantmig_immigration_restrict, legislation_count, legislation_weight, 
    impic_immigration_restrict, impic_immigration_restrict_lag1, 
    impic_immigration_restrict_lag2, impic_immigration_restrict_lag3,
    impic_immigration_restrict_lag4, impic_immigration_restrict_lead1, 
    impic_immigration_restrict_lead2, impic_immigration_restrict_lead3, 
    impic_immigration_restrict_lead4, majoritarian, net_migration, net_migration_lag1, 
    net_migration_lag2, net_migration_first_dif, partial_year_weight
  ) |>
  dplyr::mutate(legislation_weight = legislation_weight / sum(legislation_weight))
  
write.csv(panel_data, "~/papers/pledge_paper/data/rdd_pledge_panel.csv")


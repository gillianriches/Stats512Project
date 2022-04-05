# The following is the functions that prepare the crash data to be joined

# Keeps the following columns from the location file
check_location <- function(df){
  df %>%
    select(crash_id = crash_id, 
           crash_datetime = crash_datetime, 
           crash_severity_id = crash_severity_id, 
           light_condition_id = light_condition_id, 
           weather_condition_id = weather_condition_id, 
           manner_collision_id = manner_collision_id,
           roadway_surf_condition_id = roadway_surf_condition_id,
           route = route,
           milepoint = milepoint,
           county_id = county_id)
}

# Keeps the following columns from the vehicle file
check_vehicle <- function(df){
  df %>%
    select(crash_id = crash_id, 
           vehicle_num = vehicle_num, 
           vehicle_year = vehicle_year,
           travel_direction_id = travel_direction_id, 
           most_harmful_event_id = most_harmful_event_id, 
           vehicle_maneuver_id = vehicle_maneuver_id)
}

# Keeps the following columns from the rollups file
check_rollups <- function(df){
  df %>%
    select(crash_id = crash_id, 
           number_fatalities = number_fatalities, 
           number_four_injuries = number_four_injuries,
           number_three_injuries = number_three_injuries, 
           number_two_injuries = number_two_injuries,
           number_one_injuries = number_one_injuries,
           pedalcycle_involved = pedalcycle_involved, 
           pedestrian_involved = pedestrian_involved,
           motorcycle_involved = motorcycle_involved,	
           distracted_driving = distracted_driving,
           speed_related = speed_related,
           adverse_weather = adverse_weather,	
           adverse_roadway_surf_condition = adverse_roadway_surf_condition,	
           roadway_geometry_related = roadway_geometry_related,
           roadway_departure = roadway_departure,	
           overturn_rollover = overturn_rollover,	
           route_type = route_type,	
           night_dark_condition = night_dark_condition,	
           single_vehicle = single_vehicle)
}

check_aadt <- function(df){
  df %>%
    mutate(route = RT_NUM) %>%
    select(START_ACCU = START_ACCU,
           END_ACCUM = END_ACCUM,
           route,
           AADT2020 = AADT2020,
           AADT2019 = AADT2019,
           AADT2018 = AADT2018,
           AADT2017 = AADT2017,
           AADT2016 = AADT2016,
           AADT2015 = AADT2015,
           AADT2014 = AADT2014,
           DESC_ = DESC_)
}





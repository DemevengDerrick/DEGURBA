
# LOAD LIBRARIES ----------------------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, haven, forcats)

# LOAD DATA ---------------------------------------------------------------

demographics <- haven::read_sav("input/DataExercise/SPSS files/DemographicsDIST.sav")

# TRANSFORM DATA ----------------------------------------------------------



# INDICATORS CALCULATION --------------------------------------------------

##  I - EMPOWERMENT INDICATOR: Child Marriage

# Indicator Definition :
# number of women aged 18-24 who were married before age 18 / total number of women aged 18-24 * 100 

# Variables :
# - P4_SEX_P = Sex (1 - Male, 2 - Female)
# - P5_AGE_P = Age (Between 18 & 24)
# - DIST_P = District

# (a) Desegregate By District
demographics |>
  dplyr::filter(P5_AGE_P >= 18, P5_AGE_P <= 24, P4_SEX_P == 2) |>
  dplyr::group_by(DIST_P) |>
  dplyr::summarise(
    total = n(),
    married_18_above = sum(P37_AGE_FIRST_MARRAIGE_P >= 18, na.rm = T), # what is the rational for looking at marriage before 18 and marriage after 18?
    married_before_18 = sum(P37_AGE_FIRST_MARRAIGE_P < 18, na.rm = T),
    married_NA = sum(is.na(P37_AGE_FIRST_MARRAIGE_P)),
    rate_18_above = round(married_18_above / total, 2),
    rate_before_18 = round(married_before_18 / total, 2),
    rate_NA = round(married_NA / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P)
  ) |>
  dplyr::select(
    DIST_P_name,
    everything()
  ) |>
  view()


# (b) Desegregate by District & Rural/Urban

# long table
demographics |>
  dplyr::filter(P5_AGE_P >= 18, P5_AGE_P <= 24, P4_SEX_P == 2) |>
  dplyr::group_by(DIST_P, REGION_P) |>
  dplyr::summarise(
    total = n(),
    married_18_above = sum(P37_AGE_FIRST_MARRAIGE_P >= 18, na.rm = T), # what is the rational for looking at marriage before 18 and marriage after 18?
    married_before_18 = sum(P37_AGE_FIRST_MARRAIGE_P < 18, na.rm = T),
    married_NA = sum(is.na(P37_AGE_FIRST_MARRAIGE_P)),
    rate_18_above = round(married_18_above / total, 2),
    rate_before_18 = round(married_before_18 / total, 2),
    rate_NA = round(married_NA / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P),
    REGION_P_name = forcats::as_factor(REGION_P)
  ) |>
  dplyr::select(
    DIST_P_name,
    REGION_P_name,
    everything()
  ) |>
  view()

# wide table
demographics |>
  dplyr::filter(P5_AGE_P >= 18, P5_AGE_P <= 24, P4_SEX_P == 2) |>
  dplyr::group_by(DIST_P, REGION_P) |>
  dplyr::summarise(
    total = n(),
    married_18_above = sum(P37_AGE_FIRST_MARRAIGE_P >= 18, na.rm = T), # what is the rational for looking at marriage before 18 and marriage after 18?
    married_before_18 = sum(P37_AGE_FIRST_MARRAIGE_P < 18, na.rm = T),
    married_NA = sum(is.na(P37_AGE_FIRST_MARRAIGE_P)),
    rate_18_above = round(married_18_above / total, 2),
    rate_before_18 = round(married_before_18 / total, 2),
    rate_NA = round(married_NA / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P),
    REGION_P = forcats::as_factor(REGION_P)
  ) |>
  dplyr::select(
    DIST_P_name,
    REGION_P,
    everything()
  ) |>
  tidyr::pivot_wider(
    names_from =  REGION_P,
    values_from = 4:10
  ) |>
  view()


## II - EDUCATION INDICATOR: Secondary school Net Attendance Ratio (NAR)

# Indicator Definition :
# number of youth aged 14-18, attending secondary school (which means, completed grade 7-11) / total number of youth aged 14-18 * 100 
# According to DHR report: 
# - secondary school age are 14-18 years
# - secondary school grade are 8-12)

# Variables :
# - P28_HIGHEST_LEVEL_P = 
# - P27_ATTENDING_SCHOOL_P = 
# - P26_EVER_ATTEND_SCHOOL_P =

# (a) Desegregate By District
demographics |>
  dplyr::mutate(
    P28Sec = case_when(
      P28_HIGHEST_LEVEL_P %in% 7:11 ~ 1,
      TRUE ~ 2
    ),
    SecSchAtt = case_when(
      P27_ATTENDING_SCHOOL_P == 1 & P28Sec == 2 ~ 3,
      P26_EVER_ATTEND_SCHOOL_P == 2 ~ 4,
      TRUE ~ P27_ATTENDING_SCHOOL_P
    )
  ) |>
  dplyr::filter(P5_AGE_P >= 14, P5_AGE_P <= 18) |>
  dplyr::group_by(DIST_P) |>
  dplyr::summarise(
    total = n(),
    attending_sec = sum(SecSchAtt == 1, na.rm = TRUE),
    rate = round(attending_sec / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P),
  ) |>
  dplyr::select(
    DIST_P_name,
    everything()
  ) |>
  view()


# (b) Desegregate by District & Rural/Urban

# long table
demographics |>
  dplyr::mutate(
    P28Sec = case_when(
      P28_HIGHEST_LEVEL_P %in% 7:11 ~ 1,
      TRUE ~ 2
    ),
    SecSchAtt = case_when(
      P27_ATTENDING_SCHOOL_P == 1 & P28Sec == 2 ~ 3, # attending school but not in secondary
      P26_EVER_ATTEND_SCHOOL_P == 2 ~ 4, # Never attended school
      TRUE ~ P27_ATTENDING_SCHOOL_P # Else, Attending school and in secondary (1)
    )
  ) |>
  dplyr::filter(P5_AGE_P >= 14, P5_AGE_P <= 18) |>
  dplyr::group_by(DIST_P, REGION_P) |>
  dplyr::summarise(
    total = n(),
    attending_sec = sum(SecSchAtt == 1, na.rm = TRUE),
    rate = round(attending_sec / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P),
    REGION_P_name = forcats::as_factor(REGION_P)
  ) |>
  dplyr::select(
    DIST_P_name,
    REGION_P_name,
    everything()
  ) |>
  view()

# wide table
demographics |>
  dplyr::mutate(
    P28Sec = case_when(
      P28_HIGHEST_LEVEL_P %in% 7:11 ~ 1,
      TRUE ~ 2
    ),
    SecSchAtt = case_when(
      P27_ATTENDING_SCHOOL_P == 1 & P28Sec == 2 ~ 3,
      P26_EVER_ATTEND_SCHOOL_P == 2 ~ 4,
      TRUE ~ P27_ATTENDING_SCHOOL_P
    )
  ) |>
  dplyr::filter(P5_AGE_P >= 14, P5_AGE_P <= 18) |>
  dplyr::group_by(DIST_P, REGION_P) |>
  dplyr::summarise(
    total = n(),
    attending_sec = sum(SecSchAtt == 1, na.rm = TRUE),
    rate = round(attending_sec / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P),
    REGION_P = forcats::as_factor(REGION_P)
  ) |>
  dplyr::select(
    DIST_P_name,
    REGION_P,
    everything()
  ) |>
  tidyr::pivot_wider(
    names_from =  REGION_P,
    values_from = 4:6
  ) |>
  view()

## III - EMPLOYMENT INDICATOR : Youth (20-24 years of age) not in employment, education or training (NEET)

# Indicator Definition :
# number of youth aged 20-24 not in employment, education or training *100 / total number of youth aged 20-24

# Variables :
# - P32_ACTIVITY_LAST_12_MONTHS_P = 
# - P5_AGE_P = Age (20 - 24)

# (a) Desegregate By District
demographics |>
  dplyr::mutate(
    Employ = case_when(
      P32_ACTIVITY_LAST_12_MONTHS_P %in% 1:6 ~ 1,
      P32_ACTIVITY_LAST_12_MONTHS_P %in% 7:9 ~ 2,
      P32_ACTIVITY_LAST_12_MONTHS_P == 11 ~ 2,
      P32_ACTIVITY_LAST_12_MONTHS_P == 10 ~ 3
    )
  ) |>
  dplyr::filter(P5_AGE_P >= 20, P5_AGE_P <= 24) |>
  dplyr::group_by(DIST_P) |>
  summarise(
    total = n(),
    neet = sum(Employ == 2, na.rm = TRUE),
    rate = round(neet / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P),
  ) |>
  dplyr::select(
    DIST_P_name,
    everything()
  ) |>
  view()

# (b) Desegregate by District & Rural/Urban

# long table
demographics |>
  dplyr::mutate(
    Employ = case_when(
      P32_ACTIVITY_LAST_12_MONTHS_P %in% 1:6 ~ 1,
      P32_ACTIVITY_LAST_12_MONTHS_P %in% 7:9 ~ 2,
      P32_ACTIVITY_LAST_12_MONTHS_P == 11 ~ 2,
      P32_ACTIVITY_LAST_12_MONTHS_P == 10 ~ 3
    )
  ) |>
  dplyr::filter(P5_AGE_P >= 20, P5_AGE_P <= 24) |>
  dplyr::group_by(DIST_P, REGION_P) |>
  summarise(
    total = n(),
    neet = sum(Employ == 2, na.rm = TRUE),
    rate = round(neet / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P),
    REGION_P_name = forcats::as_factor(REGION_P)
  ) |>
  dplyr::select(
    DIST_P_name,
    REGION_P_name,
    everything()
  ) |>
  view()

# wide table
demographics |>
  dplyr::mutate(
    Employ = case_when(
      P32_ACTIVITY_LAST_12_MONTHS_P %in% 1:6 ~ 1,
      P32_ACTIVITY_LAST_12_MONTHS_P %in% 7:9 ~ 2,
      P32_ACTIVITY_LAST_12_MONTHS_P == 11 ~ 2,
      P32_ACTIVITY_LAST_12_MONTHS_P == 10 ~ 3
    )
  ) |>
  dplyr::filter(P5_AGE_P >= 20, P5_AGE_P <= 24) |>
  dplyr::group_by(DIST_P, REGION_P) |>
  summarise(
    total = n(),
    neet = sum(Employ == 2, na.rm = TRUE),
    rate = round(neet / total, 2)
  ) |>
  dplyr::mutate(
    DIST_P_name = forcats::as_factor(DIST_P),
    REGION_P = forcats::as_factor(REGION_P)
  ) |>
  dplyr::select(
    DIST_P_name,
    REGION_P,
    everything()
  ) |>
  tidyr::pivot_wider(
    names_from =  REGION_P,
    values_from = 4:6
  ) |>
  view()

# VISUALIZE ---------------------------------------------------------------


# EXPORT ------------------------------------------------------------------



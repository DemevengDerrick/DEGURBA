
# LOAD LIBRARIES ----------------------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, haven)

# LOAD DATA ---------------------------------------------------------------

demographics <- haven::read_sav("input/DataExercise/SPSS files/DemographicsDIST.sav")

# TRANSFORM DATA ----------------------------------------------------------



# INDICATORS CALCULATION --------------------------------------------------

##  INDICATOR: Women 18-24 with Child Marriage

# (a) Check missing value - Totally 96919 people, 46405 are missing data
# Variable Age at the first marriage: there are 46405 (47.9%) missing data
# Variable Marital Status: there are 39839 people “Never married” and 6566 missing data. These two group people did not answer “Age at the first marriage”. (39839+6566=46405)
# In this research, the 46405 people are included in the denominator, i.e. the total 96919 women at age of 18-24 is the denominator.

df_18_24 <- demographics |>
  dplyr::filter(P5_AGE_P >= 18, P5_AGE_P <= 24, P4_SEX_P == 2)

summary(df_18_24$P37_AGE_FIRST_MARRAIGE_P)

# (b) Calculate child marriage at district level, first calculate total girls 18-24 using "Filter", then calculate girls 18-24 married before 18 using "Filter".

df_18_24 |>
  dplyr::group_by(DIST_P) |>
  dplyr::summarise(
    total = n(),
    married_18_above = sum(P37_AGE_FIRST_MARRAIGE_P > 17, na.rm = T),
    married_before_18 = sum(P37_AGE_FIRST_MARRAIGE_P <= 17, na.rm = T),
    married_NA = sum(is.na(P37_AGE_FIRST_MARRAIGE_P)),
    rate_18_above = round(married_18_above / total, 2),
    rate_before_18 = round(married_before_18 / total, 2),
    rate_NA = round(married_NA / total, 2)
  ) |>
  view()


# (c)) Disagregate by Rual/Urban

# long table
df_18_24 |>
  dplyr::group_by(DIST_P, REGION_P) |>
  dplyr::summarise(
    total = n(),
    married_18_above = sum(P37_AGE_FIRST_MARRAIGE_P > 17, na.rm = T),
    married_before_18 = sum(P37_AGE_FIRST_MARRAIGE_P <= 17, na.rm = T),
    married_NA = sum(is.na(P37_AGE_FIRST_MARRAIGE_P)),
    rate_18_above = round(married_18_above / total, 2),
    rate_before_18 = round(married_before_18 / total, 2),
    rate_NA = round(married_NA / total, 2)
  ) |>
  view()

# wide table
df_18_24 |>
  dplyr::group_by(DIST_P, REGION_P) |>
  dplyr::summarise(
    total = n(),
    married_18_above = sum(P37_AGE_FIRST_MARRAIGE_P > 17, na.rm = T),
    married_before_18 = sum(P37_AGE_FIRST_MARRAIGE_P <= 17, na.rm = T),
    married_NA = sum(is.na(P37_AGE_FIRST_MARRAIGE_P)),
    rate_18_above = round(married_18_above / total, 2),
    rate_before_18 = round(married_before_18 / total, 2),
    rate_NA = round(married_NA / total, 2)
  ) |>
  tidyr::pivot_wider(
    names_from =  REGION_P,
    values_from = 3:9
  ) |>
  view()


## INDICATOR: Currently Attending Secondary School

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
  dplyr::group_by(DIST_P) %>%
  dplyr::summarise(
    total = n(),
    attending_sec = sum(SecSchAtt == 1, na.rm = TRUE),
    rate = attending_sec / total
  ) |>
  view()


## INDICATOR: NEET (Not in Education, Employment or Training) 19–24

demographics |>
  dplyr::mutate(
    Employ = case_when(
      P32_ACTIVITY_LAST_12_MONTHS_P %in% 1:6 ~ 1,
      P32_ACTIVITY_LAST_12_MONTHS_P %in% 7:9 ~ 2,
      P32_ACTIVITY_LAST_12_MONTHS_P == 11 ~ 2,
      P32_ACTIVITY_LAST_12_MONTHS_P == 10 ~ 3
    )
  ) |>
  dplyr::filter(P5_AGE_P >= 19, P5_AGE_P <= 24) |>
  dplyr::group_by(DIST_P) %>%
  summarise(
    total = n(),
    neet = sum(Employ == 2, na.rm = TRUE),
    rate = neet / total
  ) |>
  view()


# VISUALIZE ---------------------------------------------------------------


# EXPORT ------------------------------------------------------------------



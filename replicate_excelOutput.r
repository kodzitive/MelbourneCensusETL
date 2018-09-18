# Nicholas Spyrison 17/09/2018.

# packages used
library("readxl")
library("dplyr")
library("tibble")

# read data
read_excel(path ="./data/Collated data Melbourne 2016 2011 2006.xlsx",
           sheet = "2006", col_names = TRUE, na = "", trim_ws = TRUE,
           skip = 1, n_max = 100000, guess_max = min(6328, 10000)
) %>% as_tibble() -> melb06_in

# stage
dat <- melb06_in
ETL <- dat[
  , c("CD", "Population",
  "ID count (multi)", "ID count/ Pop.", # for Ethnicity
  "% Different address 5 yrs ago", # for Mobility

  # below is for generation, income, and education:
  "Father Born in Australia", "Father Born overseas",
  "Mother Born in Australia", "Mother Born overseas",
  "Before 1980", "1980 to 1990", "1991 to 2000", "2001 to 2006",
    # for Generation SI
  "$1-$149", "$150-$249", "$250-$399", "$400-$599", "$600-$799", "$800-$999",
  "$1,000-$1,299", "$1,300-$1,599", "$1,600-$1,999", "$2,000 or more",
    # for Income SI
  "Bachelor degree or higher", "Post Seconday but no university degree",
  "Certificate Level, nfd", "Certificate I & II Level", "Year 12 or equivalent",
  "Year 11 or equivalent", "Year 10 or equivalent", "Year 9 or equivalent",
  "Year 8 or below" # for Education SI
  )]
ETL <- rename(.data = ETL, "ID count" = "ID count (multi)")

# calc metric values for ethn-raw, ethn-norm, mobility-raw
ETL <- mutate(
  .data = ETL,
  `Ethnicity-raw-count` =
    if(`ID count` > 0) {`ID count`} else (NA),
  `Ethnicity-raw-normalized` =
    if(`ID count/ Pop.` > 0) {`ID count/ Pop.`} else (NA),
  `Mobility-raw-pct` =
    if(`% Different address 5 yrs ago` > 0)
      {`% Different address 5 yrs ago`} else (NA)
  )

# stage1 for metric calcs for generation, income, education
ETL <- mutate(
  .data = ETL,
  `generation_faux_total` =
    max( ("Father Born in Australia" + "Father Born overseas"),
        ("Mother Born in Australia" + "Mother Born overseas") ) +
    "Before 1980" + "1980 to 1990" + "1991 to 2000" + "2001 to 2006",
  `income_total` =
    "$1-$149" + "$150-$249" + "$250-$399" + "$400-$599" + "$600-$799" +
    "$800-$999" + "$1,000-$1,299" + "$1,300-$1,599" + "$1,600-$1,999" +
    "$2,000 or more",
  `education_total` =
    "Bachelor degree or higher" + "Post Seconday but no university degree" +
    "Certificate Level, nfd" + "Certificate I & II Level" +
    "Year 12 or equivalent" + "Year 11 or equivalent" + "Year 10 or equivalent" +
    "Year 9 or equivalent" + "Year 8 or below"
  )

# stage2 for metric calcs for generation, income, education
ETL <- mutate(
  .data = ETL,
  # generation:
  `pct_father_born_aus` = "Father Born in Australia" / `generation_faux_total`,
  `pct_father_born_os` = "Father Born overseas" / `generation_faux_total`,
  `pct_mother_born_aus` = "Mother Born in Australia" / `generation_faux_total`,
  `pct_mother_born_os` = "Mother Born overseas" / `generation_faux_total`,
  `pct_before_1980` = "Before 1980" / `generation_faux_total`,
  `pct_1980_to_1990` = "1980 to 1990" / `generation_faux_total`,
  `pct_1991_to_2000` = "1991 to 2000" / `generation_faux_total`,
  `pct_2001_to_2006` = "2001 to 2006" / `generation_faux_total`,
  # income:
  `pct_$1` = "$1-$149" / `income_total`,
  `pct_$150` = "$150-$249" / `income_total`,
  `pct_$250` = "$250-$399" / `income_total`,
  `pct_$400` = "$400-$599" / `income_total`,
  `pct_$600` = "$600-$799" / `income_total`,
  `pct_$800` = "$800-$999" / `income_total`,
  `pct_$1000` = "$1,000-$1,299" / `income_total`,
  `pct_$1300` = "$1,300-$1,599" / `income_total`,
  `pct_$1600` = "$1,600-$1,999" / `income_total`,
  `pct_$2000` = "$2,000 or more" / `income_total`,
  # education:
  `pct_bachelor` = "Bachelor degree or higher" / `education_total`,
  `pct_post_secondary` =
    "Post Seconday but no university degree" / `education_total`,
  `pct_derived_hs` = ("Certificate Level, nfd" + "Certificate I & II Level"
                  + "Year 12 or equivalent") / `education_total`,
  `pct_lt_hs` = ("Year 11 or equivalent" + "Year 10 or equivalent" +
                "Year 9 or equivalent" + "Year 8 or below") / `education_total`
)

# calc metric values for generation, income, education
ETL <- mutate(
  .data = ETL,
  `Generation-raw-SI` =
    if(`generation_faux_total` == 0) {NA}
    else (1-(`pct_father_born_aus`^2 + `pct_father_born_os`^2 +
               `pct_mother_born_aus`^2 + `pct_mother_born_os`^2 +
               `pct_before_1980`^2 + `pct_1980_to_1990`^2 +
               `pct_1991_to_2000`^2 + `pct_2001_to_2006`^2)),
  `Income-raw-SI` =
    if(`income_total` == 0) {NA}
    else (1-(`pct_$1`^2 + `pct_$150`^2 + `pct_$250`^2 + `pct_$400`^2 +
               `pct_$600`^2 + `pct_$800`^2 + `pct_$1000`^2 + `pct_$1300`^2 +
               `pct_$1600`^2 + `pct_$2000`^2)),
  `Education-raw-SI` =
    if(`education_total` == 0) {NA}
    else (1-(`pct_bachelor`^2 + `pct_post_secondary`^2 + `pct_derived_hs`^2 +
               `pct_lt_hs`^2))
)

# create tetile 'index' on metric values
ETL <- mutate(
  .data = ETL,
  `Ethnicity-index` = ntile(`Ethnicity-raw-count`, 3),
  `Ethnicity-norm-index` = ntile(`Ethnicity-raw-normalized`, 3),
  `Mobility-index` = ntile(`Mobility-raw-pct`, 3),
  `Generation-index` = ntile(`Generation-raw-SI`, 3),
  `Income-index` = ntile(`Income-raw-SI`, 3),
  `Education-index` = ntile(`Education-raw-SI`, 3)
)

# reorder by column name
melb06_out <- ETL[,
  c("CD", "Population", "Ethnicity-raw-count", "Ethnicity-index",
    "Ethnicity-raw-normalized", "Ethnicity-norm-index", "Mobility-raw-pct",
    "Mobility-index", "Generation-raw-SI", "Generation-index", "Income-raw-SI",
    "Income-index", "Education-raw-SI", "Education-index")
  ]

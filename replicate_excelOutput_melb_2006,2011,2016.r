# Nicholas Spyrison 17/09/2018 - 19/09/2018.

# packages used
library("readxl")
library("dplyr")
library("tibble")

# read data
read_excel(path = "./data/Collated data Melbourne 2016 2011 2006.xlsx",
           sheet = "2006", col_names = TRUE, na = "", trim_ws = TRUE,
           skip = 1, n_max = 99999, guess_max = 6328
) %>% as_tibble() -> melb06_in

# stage
ETL <- melb06_in
ETL <- ETL[
  , c("CD", "Population", # Regional grain
  "ID count (multi)", "ID count/ Pop.", # for Ethnicity
  "% Different address 5 yrs ago", # for Mobility
  # for Generation SI:
  "Father Born in Australia", "Father Born overseas",
  "Mother Born in Australia", "Mother Born overseas",
  "Before 1980", "1980 to 1990", "1991 to 2000", "2001 to 2006",
  # for Income SI:
  "$1-$149", "$150-$249", "$250-$399", "$400-$599", "$600-$799", "$800-$999",
  "$1,000-$1,299", "$1,300-$1,599", "$1,600-$1,999", "$2,000 or more",
  # for Education SI:
  "Bachelor degree or higher", "Post Seconday but no university degree",
  "Certificate Level, nfd", "Certificate I & II Level", "Year 12 or equivalent",
  "Year 11 or equivalent", "Year 10 or equivalent", "Year 9 or equivalent",
  "Year 8 or below", "Did not go to school"
  )]
ETL <- rename(.data = ETL, `ID count` = `ID count (multi)`)

# calc metric values for ethn-raw, ethn-norm, mobility-raw
ETL <- mutate(
  .data = ETL,
  `Ethnicity-raw-count` =
    ifelse(`ID count` > 0, `ID count`, NA),
  `Ethnicity-raw-normalized` =
    ifelse(`ID count/ Pop.` > 0, `ID count/ Pop.`, NA),
  `Mobility-raw-pct` =
    ifelse(`% Different address 5 yrs ago` > 0,
           `% Different address 5 yrs ago`, NA)
  )

# stage1 for metric calcs for generation, income, education
ETL <- mutate(
  .data = ETL,
  `generation_faux_total` =
    max( (`Father Born in Australia` + `Father Born overseas`),
        (`Mother Born in Australia` + `Mother Born overseas`)
        ) +
    `Before 1980` + `1980 to 1990` + `1991 to 2000` + `2001 to 2006`,
  `income_total` =
    `$1-$149` + `$150-$249` + `$250-$399` + `$400-$599` + `$600-$799` +
    `$800-$999` + `$1,000-$1,299` + `$1,300-$1,599` + `$1,600-$1,999` +
    `$2,000 or more`,
  `education_total` =
    `Bachelor degree or higher` + `Post Seconday but no university degree` +
    `Certificate Level, nfd` + `Certificate I & II Level` +
    `Year 12 or equivalent` + `Year 11 or equivalent` + `Year 10 or equivalent` +
    `Year 9 or equivalent` + `Year 8 or below` + `Did not go to school`
  )

# stage2 for metric calcs for generation, income, education
ETL <- mutate(
  .data = ETL,
  # generation:
  `pct_father_born_aus` = `Father Born in Australia` / `generation_faux_total`,
  `pct_father_born_os` = `Father Born overseas` / `generation_faux_total`,
  `pct_mother_born_aus` = `Mother Born in Australia` / `generation_faux_total`,
  `pct_mother_born_os` = `Mother Born overseas` / `generation_faux_total`,
  `pct_before_1980` = `Before 1980` / `generation_faux_total`,
  `pct_1980_to_1990` = `1980 to 1990` / `generation_faux_total`,
  `pct_1991_to_2000` = `1991 to 2000` / `generation_faux_total`,
  `pct_2001_to_2006` = `2001 to 2006` / `generation_faux_total`,
  # income:
  `pct_$1` = `$1-$149` / `income_total`,
  `pct_$150` = `$150-$249` / `income_total`,
  `pct_$250` = `$250-$399` / `income_total`,
  `pct_$400` = `$400-$599` / `income_total`,
  `pct_$600` = `$600-$799` / `income_total`,
  `pct_$800` = `$800-$999` / `income_total`,
  `pct_$1000` = `$1,000-$1,299` / `income_total`,
  `pct_$1300` = `$1,300-$1,599` / `income_total`,
  `pct_$1600` = `$1,600-$1,999` / `income_total`,
  `pct_$2000` = `$2,000 or more` / `income_total`,
  # education:
  `pct_bachelor` = `Bachelor degree or higher` / `education_total`,
  `pct_post_secondary` =
    `Post Seconday but no university degree` / `education_total`,
  `pct_derived_hs` = (`Certificate Level, nfd` + `Certificate I & II Level`
                  + `Year 12 or equivalent`) / `education_total`,
  `pct_lt_hs` = (`Year 11 or equivalent` + `Year 10 or equivalent` +
                   `Year 9 or equivalent` + `Year 8 or below` +
                   `Did not go to school`) / `education_total`
)

# calc metric values for generation, income, education
ETL <- mutate(
  .data = ETL,
  `Generation-raw-SI` =
    ifelse(`generation_faux_total` == 0, NA,
           (1-(`pct_father_born_aus`^2 + `pct_father_born_os`^2 +
               `pct_mother_born_aus`^2 + `pct_mother_born_os`^2 +
               `pct_before_1980`^2 + `pct_1980_to_1990`^2 +
               `pct_1991_to_2000`^2 + `pct_2001_to_2006`^2)
            )
    ),
  `Income-raw-SI` =
    ifelse(`income_total` == 0, NA,
           (1-(`pct_$1`^2 + `pct_$150`^2 + `pct_$250`^2 + `pct_$400`^2 +
               `pct_$600`^2 + `pct_$800`^2 + `pct_$1000`^2 + `pct_$1300`^2 +
               `pct_$1600`^2 + `pct_$2000`^2)
            )
    ),
  `Education-raw-SI` =
    ifelse(`education_total` == 0, NA,
           (1-(`pct_bachelor`^2 + `pct_post_secondary`^2 + `pct_derived_hs`^2 +
                 `pct_lt_hs`^2)
           )
    )
)

# create tetile 'index' on metric values
ETL <- mutate(
  .data = ETL,
  `Ethnicity-index`      = ntile(`Ethnicity-raw-count`, 3),
  `Ethnicity-norm-index` = ntile(`Ethnicity-raw-normalized`, 3),
  `Mobility-index`       = ntile(`Mobility-raw-pct`, 3),
  `Generation-index`     = ntile(`Generation-raw-SI`, 3),
  `Income-index`         = ntile(`Income-raw-SI`, 3),
  `Education-index`      = ntile(`Education-raw-SI`, 3)
)

# reorder by column name
melb06_out <- ETL[,
  c("CD", "Population", "Ethnicity-raw-count", "Ethnicity-index",
    "Ethnicity-raw-normalized", "Ethnicity-norm-index", "Mobility-raw-pct",
    "Mobility-index", "Generation-raw-SI", "Generation-index", "Income-raw-SI",
    "Income-index", "Education-raw-SI", "Education-index")
  ]

write.csv(melb06_out, row.names = FALSE,
          file = "./output/Melbourne_2006_diversityindices.csv")

# :Melbourne 2006
# ===== Melb 2006 above. Melb 2011 below ===== #
# Melbourne 2011:

# packages used
library("readxl")
library("dplyr")
library("tibble")

# read data
read_excel(path = "./data/Collated data Melbourne 2016 2011 2006.xlsx",
           sheet = "2011", col_names = TRUE, na = "", trim_ws = TRUE,
           skip = 1, n_max = 99999, guess_max = 9660
) %>% as_tibble() -> melb11_in

# stage
ETL <- melb11_in
ETL <- ETL[
  , c("SA1", "Total population", # Regional grain
      "ID count multi", "ID count/population", # for Ethnicity
      "% Different address 5 yrs ago", # for Mobility
      # for Generation SI:
      "Both parents born in Australia",
      "A-born with at least one parent born OS",
      "Before 1980", "1980 to 1990", "1991 to 2000", "2001 to 2011",
      # for Income SI:
      "$1-$199 ($1-$10,399)", "$200-$299 ($10,400-$15,599)",
      "$300-$399 ($15,600-$20,799)", "$400-$599 ($20,800-$31,199)",
      "$600-$799 ($31,200-$41,599)", "$800-$999 ($41,600-$51,999)",
      "$1,000-$1,249 ($52,000-$64,999)", "$1,250-$1,499 ($65,000-$77,999)",
      "$1,500-$1,999 ($78,000-$103,999)", "$2,000 or more ($104,000 or more)",
      # for Education SI:
      "Bachelor degree or higher", "Post Seconday but no university degree",
      "Certificate Level, nfd", "Certificate I & II Level",
      "Year 12 or equivalent", "Year 11 or equivalent", "Year 10 or equivalent",
      "Year 9 or equivalent", "Year 8 or below", "Did not go to school"
  )]
ETL <- rename(.data = ETL,
              `ID count` = `ID count multi`,
              `Population` = `Total population`)

# calc metric values for ethn-raw, ethn-norm, mobility-raw
ETL <- mutate(
  .data = ETL,
  `Ethnicity-raw-count` =
    ifelse(`ID count` > 0, `ID count`, NA),
  `Ethnicity-raw-normalized` =
    ifelse(`ID count/population` > 0, `ID count/population`, NA),
  `Mobility-raw-pct` =
    ifelse(`% Different address 5 yrs ago` > 0,
           `% Different address 5 yrs ago`, NA)
)

# stage1 for metric calcs for generation, income, education
ETL <- mutate(
  .data = ETL,
  `generation_faux_total` =
    `Both parents born in Australia`, `A-born with at least one parent born OS`+
    `Before 1980` + `1980 to 1990` + `1991 to 2000` + `2001 to 2011`,
  `income_total` =
    `$1-$199 ($1-$10,399)` + `$200-$299 ($10,400-$15,599)` +
    `$300-$399 ($15,600-$20,799)` + `$400-$599 ($20,800-$31,199)` +
    `$600-$799 ($31,200-$41,599)` + `$800-$999 ($41,600-$51,999)` +
    `$1,000-$1,249 ($52,000-$64,999)` + `$1,250-$1,499 ($65,000-$77,999)` +
    `$1,500-$1,999 ($78,000-$103,999)` + `$2,000 or more ($104,000 or more)`,
  `education_total` =
    `Bachelor degree or higher` + `Post Seconday but no university degree` +
    `Certificate Level, nfd` + `Certificate I & II Level` +
    `Year 12 or equivalent` + `Year 11 or equivalent` + `Year 10 or equivalent`+
    `Year 9 or equivalent` + `Year 8 or below` + `Did not go to school`
)

# stage2 for metric calcs for generation, income, education
ETL <- mutate(
  .data = ETL,
  # generation:
  `pct_both_born_aus` =
    `Both parents born in Australia` / `generation_faux_total`,
  `pct_one+_born_os` =
    `A-born with at least one parent born OS` / `generation_faux_total`,
  `pct_before_1980` = `Before 1980` / `generation_faux_total`,
  `pct_1980_to_1990` = `1980 to 1990` / `generation_faux_total`,
  `pct_1991_to_2000` = `1991 to 2000` / `generation_faux_total`,
  `pct_2001_to_2011` = `2001 to 2011` / `generation_faux_total`,
  # income:
  `pct_$1` = `$1-$199 ($1-$10,399)` / `income_total`,
  `pct_$200` = `$200-$299 ($10,400-$15,599)` / `income_total`,
  `pct_$300` = `$300-$399 ($15,600-$20,799)` / `income_total`,
  `pct_$400` = `$400-$599 ($20,800-$31,199)` / `income_total`,
  `pct_$600` = `$600-$799 ($31,200-$41,599)` / `income_total`,
  `pct_$800` = `$800-$999 ($41,600-$51,999)` / `income_total`,
  `pct_$1000` = `$1,000-$1,249 ($52,000-$64,999)` / `income_total`,
  `pct_$1250` = `$1,250-$1,499 ($65,000-$77,999)` / `income_total`,
  `pct_$1500` = `$1,500-$1,999 ($78,000-$103,999)` / `income_total`,
  `pct_$2000` = `$2,000 or more ($104,000 or more)` / `income_total`,
  # education:
  `pct_bachelor` = `Bachelor degree or higher` / `education_total`,
  `pct_post_secondary` =
    `Post Seconday but no university degree` / `education_total`,
  `pct_derived_hs` = (`Certificate Level, nfd` + `Certificate I & II Level`
                      + `Year 12 or equivalent`) / `education_total`,
  `pct_lt_hs` = (`Year 11 or equivalent` + `Year 10 or equivalent` +
                   `Year 9 or equivalent` + `Year 8 or below` +
                   `Did not go to school`) / `education_total`
)

# calc metric values for generation, income, education
ETL <- mutate(
  .data = ETL,
  `Generation-raw-SI` =
    ifelse(`generation_faux_total` == 0, NA,
           (1-(`pct_both_born_aus`^2 + `pct_one+_born_os`^2 +
                 `pct_before_1980`^2 + `pct_1980_to_1990`^2 +
                 `pct_1991_to_2000`^2 + `pct_2001_to_2011`^2)
           )
    ),
  `Income-raw-SI` =
    ifelse(`income_total` == 0, NA,
           (1-(`pct_$1`^2 + `pct_$200`^2 + `pct_$300`^2 + `pct_$400`^2 +
                 `pct_$600`^2 + `pct_$800`^2 + `pct_$1000`^2 + `pct_$1250`^2 +
                 `pct_$1500`^2 + `pct_$2000`^2)
           )
    ),
  `Education-raw-SI` =
    ifelse(`education_total` == 0, NA,
           (1-(`pct_bachelor`^2 + `pct_post_secondary`^2 + `pct_derived_hs`^2 +
                 `pct_lt_hs`^2)
           )
    )
)

# create tetile 'index' on metric values
ETL <- mutate(
  .data = ETL,
  `Ethnicity-index`      = ntile(`Ethnicity-raw-count`, 3),
  `Ethnicity-norm-index` = ntile(`Ethnicity-raw-normalized`, 3),
  `Mobility-index`       = ntile(`Mobility-raw-pct`, 3),
  `Generation-index`     = ntile(`Generation-raw-SI`, 3),
  `Income-index`         = ntile(`Income-raw-SI`, 3),
  `Education-index`      = ntile(`Education-raw-SI`, 3)
)

# reorder by column name
melb11_out <- ETL[
  ,c("SA1", "Population", "Ethnicity-raw-count", "Ethnicity-index",
     "Ethnicity-raw-normalized", "Ethnicity-norm-index", "Mobility-raw-pct",
     "Mobility-index", "Generation-raw-SI", "Generation-index", "Income-raw-SI",
     "Income-index", "Education-raw-SI", "Education-index")
  ]

write.csv(melb11_out, row.names = FALSE,
          file = "./output/Melbourne_2011_diversityindices.csv")

# :Melbourne 2011
# ===== Melb 2011 above. Melb 2016 below ===== #
# Melbourne 2016:

# packages used
library("readxl")
library("dplyr")
library("tibble")

# read data
read_excel(path = "./data/Collated data Melbourne 2016 2011 2006.xlsx",
           sheet = "2016", col_names = TRUE, na = "", trim_ws = TRUE,
           skip = 1, n_max = 99999, guess_max = 10290
) %>% as_tibble() -> melb16_in

# stage
ETL <- melb16_in
ETL <- ETL[
  , c("SA1", "Total Persons", # Regional grain
      "ID count (ANC multi)", "ID count/ Pop.", # for Ethnicity
      "% Different address 5 yrs ago", # for Mobility
      # for Generation SI:
      "Both parents born in Australia",
      "A-born with at least one parent born OS", "Before 1980",
      "1980-1990", "1991-2000", "2001-2010", "2011-2016",
      # for Income SI:
      "$1-$149 ($1-$7,799)", "$150-$299 ($7,800-$15,599)",
      "$300-$399 ($15,600-$20,799)", "$400-$499 ($20,800-$25,999)",
      "$500-$649 ($26,000-$33,799)", "$650-$799 ($33,800-$41,599)",
      "$800-$999 ($41,600-$51,999)", "$1,000-$1,249 ($52,000-$64,999)",
      "$1,250-$1,499 ($65,000-$77,999)", "$1,500-$1,749 ($78,000-$90,999)",
      "$1,750-$1,999 ($91,000-$103,999)", "$2,000-$2,999 ($104,000-$155,999)",
      "$3,000 or more ($156,000 or more)",
      # for Education SI:
      "Bachelor degree or higher", "Post Seconday but no university degree",
      "Certificate Level, nfd", "Certificate I & II Level",
      "Year 12 or equivalent", "Year 11 or equivalent", "Year 10 or equivalent",
      "Year 9 or equivalent", "Year 8 or below", "Did not go to school"
  )]
ETL <- rename(.data = ETL,
              `ID count` = `ID count (ANC multi)`,
              `Population` = `Total Persons`)

# calc metric values for ethn-raw, ethn-norm, mobility-raw
ETL <- mutate(
  .data = ETL,
  `Ethnicity-raw-count` =
    ifelse(`ID count` > 0, `ID count`, NA),
  `Ethnicity-raw-normalized` =
    ifelse(`ID count/ Pop.` > 0, `ID count/ Pop.`, NA),
  `Mobility-raw-pct` =
    ifelse(`% Different address 5 yrs ago` > 0,
           `% Different address 5 yrs ago`, NA)
)

# stage1 for metric calcs for generation, income, education
ETL <- mutate(
  .data = ETL,
  `generation_faux_total` =
    `Both parents born in Australia`, `A-born with at least one parent born OS`+
    `Before 1980` + `1980-1990` + `1991-2000` + `2001-2010` + `2011-2016`,
  `income_total` =
    `$1-$149 ($1-$7,799)` + `$150-$299 ($7,800-$15,599)` +
    `$300-$399 ($15,600-$20,799)` + `$400-$499 ($20,800-$25,999)` +
    `$500-$649 ($26,000-$33,799)` + `$650-$799 ($33,800-$41,599)` +
    `$800-$999 ($41,600-$51,999)` + `$1,000-$1,249 ($52,000-$64,999)` +
    `$1,250-$1,499 ($65,000-$77,999)` + `$1,500-$1,749 ($78,000-$90,999)` +
    `$2,000-$2,999 ($104,000-$155,999)` + `$3,000 or more ($156,000 or more)`,
  `education_total` =
    `Bachelor degree or higher` + `Post Seconday but no university degree` +
    `Certificate Level, nfd` + `Certificate I & II Level` +
    `Year 12 or equivalent` + `Year 11 or equivalent` + `Year 10 or equivalent`+
    `Year 9 or equivalent` + `Year 8 or below` + `Did not go to school`
)

# stage2 for metric calcs for generation, income, education
ETL <- mutate(
  .data = ETL,
  # generation:
  `pct_both_born_aus` =
    `Both parents born in Australia` / `generation_faux_total`,
  `pct_one+_born_os` =
    `A-born with at least one parent born OS` / `generation_faux_total`,
  `pct_before_1980` = `Before 1980` / `generation_faux_total`,
  `pct_1980_to_1990` = `1980-1990` / `generation_faux_total`,
  `pct_1991_to_2000` = `1991-2000` / `generation_faux_total`,
  `pct_2001_to_2010` = `2001-2010` / `generation_faux_total`,
  `pct_2011_to_2016` = `2011-2016` / `generation_faux_total`,
  # income:
  `pct_$1` = `$1-$149 ($1-$7,799)` / `income_total`,
  `pct_$150` = `$150-$299 ($7,800-$15,599)` / `income_total`,
  `pct_$300` = `$300-$399 ($15,600-$20,799)` / `income_total`,
  `pct_$400` = `$400-$499 ($20,800-$25,999)` / `income_total`,
  `pct_$500` = `$500-$649 ($26,000-$33,799)` / `income_total`,
  `pct_$650` = `$650-$799 ($33,800-$41,599)` / `income_total`,
  `pct_$800` = `$800-$999 ($41,600-$51,999)` / `income_total`,
  `pct_$1000` = `$1,000-$1,249 ($52,000-$64,999)` / `income_total`,
  `pct_$1250` = `$1,250-$1,499 ($65,000-$77,999)` / `income_total`,
  `pct_$1500` = `$1,500-$1,749 ($78,000-$90,999)` / `income_total`,
  `pct_$1750` = `$1,750-$1,999 ($91,000-$103,999)` / `income_total`,
  `pct_$2000` = `$2,000-$2,999 ($104,000-$155,999)` / `income_total`,
  `pct_$3000` = `$3,000 or more ($156,000 or more)` / `income_total`,
  # education:
  `pct_bachelor` = `Bachelor degree or higher` / `education_total`,
  `pct_post_secondary` =
    `Post Seconday but no university degree` / `education_total`,
  `pct_derived_hs` = (`Certificate Level, nfd` + `Certificate I & II Level`
                      + `Year 12 or equivalent`) / `education_total`,
  `pct_lt_hs` = (`Year 11 or equivalent` + `Year 10 or equivalent` +
                   `Year 9 or equivalent` + `Year 8 or below` +
                   `Did not go to school`) / `education_total`
)

# calc metric values for generation, income, education
ETL <- mutate(
  .data = ETL,
  `Generation-raw-SI` =
    ifelse(`generation_faux_total` == 0, NA,
           (1-(`pct_both_born_aus`^2 + `pct_one+_born_os`^2 +
                 `pct_before_1980`^2 + `pct_1980_to_1990`^2 +
                 `pct_1991_to_2000`^2 + `pct_2001_to_2010`^2+
                 `pct_2011_to_2016`^2)
           )
    ),
  `Income-raw-SI` =
    ifelse(`income_total` == 0, NA,
           (1-(`pct_$1`^2 + `pct_$150`^2 + `pct_$300`^2 + `pct_$400`^2 +
                 `pct_$500`^2 + `pct_$650`^2 + `pct_$800`^2 + `pct_$1000`^2 +
                 `pct_$1250`^2 + `pct_$1500`^2 + `pct_$1750`^2 + `pct_$2000`^2 +
                 `pct_$3000`^2)
           )
    ),
  `Education-raw-SI` =
    ifelse(`education_total` == 0, NA,
           (1-(`pct_bachelor`^2 + `pct_post_secondary`^2 + `pct_derived_hs`^2 +
                 `pct_lt_hs`^2)
           )
    )
)

# create tetile 'index' on metric values
ETL <- mutate(
  .data = ETL,
  `Ethnicity-index`      = ntile(`Ethnicity-raw-count`, 3),
  `Ethnicity-norm-index` = ntile(`Ethnicity-raw-normalized`, 3),
  `Mobility-index`       = ntile(`Mobility-raw-pct`, 3),
  `Generation-index`     = ntile(`Generation-raw-SI`, 3),
  `Income-index`         = ntile(`Income-raw-SI`, 3),
  `Education-index`      = ntile(`Education-raw-SI`, 3)
)

# reorder by column name
melb16_out <- ETL[
  ,c("SA1", "Population", "Ethnicity-raw-count", "Ethnicity-index",
     "Ethnicity-raw-normalized", "Ethnicity-norm-index", "Mobility-raw-pct",
     "Mobility-index", "Generation-raw-SI", "Generation-index", "Income-raw-SI",
     "Income-index", "Education-raw-SI", "Education-index")
  ]

write.csv(x = melb16_out, row.names = FALSE,
          file = "./output/Melbourne_2016_diversityindices.csv")

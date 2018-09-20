# Nicholas Spyrison 17/09/2018 - 20/09/2018.

# packages used
library("readxl")
library("dplyr")
library("tibble")

# read data
readxl::read_excel(path = "./data/Collated data Melbourne 2016 2011 2006 updated Sept 20.xlsx",
           sheet = "2001", col_names = TRUE, na = "", trim_ws = TRUE,
           skip = 1, n_max = 99999, guess_max = 5842
) %>% as_tibble() -> melb01_in

# stage
ETL <- melb01_in
ETL <- ETL[
  , c("CD", "Population", # Regional grain
      "ID count", "ID count/ Pop.", # for Ethnicity
      "% Different address 5 yrs ago", # for Mobility
      # for Generation SI:
      "Both parents born in Australia",
      "One or both parents born overseas",
      "Before 1986", "Arrived 1986-1990", "Arrived 1991-2001",
      # for Income SI:
      ",$1 - $39,", ",$40 - $79,", ",$80 - $119,", ",$120 - $159,",
      ",$160 - $199,", ",$200 - $299,", ",$300 - $399,", ",$400 - $499,",
      ",$500 - $599,", ",$600 - $699,", ",$700 - $799,", ",$800 - $999,",
      ",$1,000 - $1,499,", ",$1,500 or more,",
      # for Education SI:
      "Bachelor degree or higher", "Post secondary, but no Univeristy degree",
      "Year 12 or equivalent", "Year 11 or equivalent", "Year 10 or equivalent",
      "Year 9 or equivalent", "Year 8 or below", "Did not go to school"
  )]

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
    `Both parents born in Australia`, `One or both parents born overseas` +
    `Before 1986` + `Arrived 1986-1990` + `Arrived 1991-2001`,
  `income_total` =
    `,$1 - $39,` + `,$40 - $79,` + `,$80 - $119,` + `,$120 - $159,` +
    `,$160 - $199,` + `,$200 - $299,` + `,$300 - $399,` + `,$400 - $499,` +
    `,$500 - $599,` + `,$600 - $699,` + `,$700 - $799,` + `,$800 - $999,` +
    `,$1,000 - $1,499,` + `,$1,500 or more,`,
  `education_total` =
    `Bachelor degree or higher` + `Post secondary, but no Univeristy degree` +
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
    `One or both parents born overseas` / `generation_faux_total`,
  `pct_before_1986` = `Before 1986` / `generation_faux_total`,
  `pct_1986_to_1990` = `Arrived 1986-1990` / `generation_faux_total`,
  `pct_1991_to_2001` = `Arrived 1991-2001` / `generation_faux_total`,
  # income:
  `pct_$1` = `,$1 - $39,` / `income_total`,
  `pct_$40` = `,$40 - $79,` / `income_total`,
  `pct_$80` = `,$80 - $119,` / `income_total`,
  `pct_$120` = `,$120 - $159,` / `income_total`,
  `pct_$160` = `,$160 - $199,` / `income_total`,
  `pct_$200` = `,$200 - $299,` / `income_total`,
  `pct_$300` = `,$300 - $399,` / `income_total`,
  `pct_$400` = `,$400 - $499,` / `income_total`,
  `pct_$500` = `,$500 - $599,` / `income_total`,
  `pct_$600` = `,$600 - $699,` / `income_total`,
  `pct_$700` = `,$700 - $799,` / `income_total`,
  `pct_$800` = `,$800 - $999,` / `income_total`,
  `pct_$1000` = `,$1,000 - $1,499,` / `income_total`,
  `pct_$1500` = `,$1,500 or more,` / `income_total`,
  # education:
  `pct_bachelor` = `Bachelor degree or higher` / `education_total`,
  `pct_post_secondary` =
    `Post secondary, but no Univeristy degree` / `education_total`,
  `pct_derived_hs` = `Year 12 or equivalent` / `education_total`,
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
                 `pct_before_1986`^2 + `pct_1986_to_1990`^2 +
                 `pct_1991_to_2001`^2)
           )
    ),
  `Income-raw-SI` =
    ifelse(`income_total` == 0, NA,
           (1-(`pct_$1`^2 + `pct_$40`^2 + `pct_$80`^2 + `pct_$120`^2 +
                 `pct_$120`^2 + `pct_$200`^2 + `pct_$300`^2 + `pct_$400`^2 +
                 `pct_$500`^2 + `pct_$600`^2 + `pct_$700`^2 + `pct_$800`^2 +
                 `pct_$1000`^2 + `pct_$1500`^2)
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
melb01_out <- ETL[
  ,c("CD", "Population", "Ethnicity-raw-count", "Ethnicity-index",
     "Ethnicity-raw-normalized", "Ethnicity-norm-index", "Mobility-raw-pct",
     "Mobility-index", "Generation-raw-SI", "Generation-index", "Income-raw-SI",
     "Income-index", "Education-raw-SI", "Education-index")
  ]

write.csv(melb01_out, row.names = TRUE,
          file = "./output/Melbourne_2001_diversityindices.csv")

# :Melbourne 2001
# ===== Melb 2001 above. Sydn 2001 below ===== #
# Sydn 2001:

# packages used
library("readxl")
library("dplyr")
library("tibble")

# read data
read_excel(path = "./data/Collated data Sydney 2016 2011 2006 2001 as of Sept 20 w addr 5 yrs ago.xlsx",
           sheet = "2001", col_names = TRUE, na = "", trim_ws = TRUE,
           skip = 1, n_max = 99999, guess_max = 10290
) %>% as_tibble() -> sydn01_in

# stage
ETL <- sydn01_in
ETL <- ETL[
  , c("CD", "Population", # Regional grain
      "ID count", "ID count/ Pop.", # for Ethnicity
      "% Different address 5 yrs ago", # for Mobility
      # for Generation SI:
      "Both parents born in Australia",
      "One or both parents born overseas",
      "Before 1986", "Arrived 1986-1990", "Arrived 1991-2001",
      # for Income SI:
      ",$1 - $39,", ",$40 - $79,", ",$80 - $119,", ",$120 - $159,",
      ",$160 - $199,", ",$200 - $299,", ",$300 - $399,", ",$400 - $499,",
      ",$500 - $599,", ",$600 - $699,", ",$700 - $799,", ",$800 - $999,",
      ",$1,000 - $1,499,", ",$1,500 or more,",
      # for Education SI:
      "Bachelor degree or higher", "Post secondary, but no Univeristy degree",
      "Year 12 or equivalent", "Year 11 or equivalent", "Year 10 or equivalent",
      "Year 9 or equivalent", "Year 8 or below", "Did not go to school"
  )]

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
    `Both parents born in Australia`, `One or both parents born overseas` +
    `Before 1986` + `Arrived 1986-1990` + `Arrived 1991-2001`,
  `income_total` =
    `,$1 - $39,` + `,$40 - $79,` + `,$80 - $119,` + `,$120 - $159,` +
    `,$160 - $199,` + `,$200 - $299,` + `,$300 - $399,` + `,$400 - $499,` +
    `,$500 - $599,` + `,$600 - $699,` + `,$700 - $799,` + `,$800 - $999,` +
    `,$1,000 - $1,499,` + `,$1,500 or more,`,
  `education_total` =
    `Bachelor degree or higher` + `Post secondary, but no Univeristy degree` +
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
    `One or both parents born overseas` / `generation_faux_total`,
  `pct_before_1986` = `Before 1986` / `generation_faux_total`,
  `pct_1986_to_1990` = `Arrived 1986-1990` / `generation_faux_total`,
  `pct_1991_to_2001` = `Arrived 1991-2001` / `generation_faux_total`,
  # income:
  `pct_$1` = `,$1 - $39,` / `income_total`,
  `pct_$40` = `,$40 - $79,` / `income_total`,
  `pct_$80` = `,$80 - $119,` / `income_total`,
  `pct_$120` = `,$120 - $159,` / `income_total`,
  `pct_$160` = `,$160 - $199,` / `income_total`,
  `pct_$200` = `,$200 - $299,` / `income_total`,
  `pct_$300` = `,$300 - $399,` / `income_total`,
  `pct_$400` = `,$400 - $499,` / `income_total`,
  `pct_$500` = `,$500 - $599,` / `income_total`,
  `pct_$600` = `,$600 - $699,` / `income_total`,
  `pct_$700` = `,$700 - $799,` / `income_total`,
  `pct_$800` = `,$800 - $999,` / `income_total`,
  `pct_$1000` = `,$1,000 - $1,499,` / `income_total`,
  `pct_$1500` = `,$1,500 or more,` / `income_total`,
  # education:
  `pct_bachelor` = `Bachelor degree or higher` / `education_total`,
  `pct_post_secondary` =
    `Post secondary, but no Univeristy degree` / `education_total`,
  `pct_derived_hs` = `Year 12 or equivalent` / `education_total`,
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
                 `pct_before_1986`^2 + `pct_1986_to_1990`^2 +
                 `pct_1991_to_2001`^2)
           )
    ),
  `Income-raw-SI` =
    ifelse(`income_total` == 0, NA,
           (1-(`pct_$1`^2 + `pct_$40`^2 + `pct_$80`^2 + `pct_$120`^2 +
                 `pct_$120`^2 + `pct_$200`^2 + `pct_$300`^2 + `pct_$400`^2 +
                 `pct_$500`^2 + `pct_$600`^2 + `pct_$700`^2 + `pct_$800`^2 +
                 `pct_$1000`^2 + `pct_$1500`^2)
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
sydn01_out <- ETL[
  ,c("CD", "Population", "Ethnicity-raw-count", "Ethnicity-index",
     "Ethnicity-raw-normalized", "Ethnicity-norm-index", "Mobility-raw-pct",
     "Mobility-index", "Generation-raw-SI", "Generation-index", "Income-raw-SI",
     "Income-index", "Education-raw-SI", "Education-index")
  ]

write.csv(x = sydn01_out, row.names = TRUE,
          file = "./output/Sydney_2001_diversityindices.csv")

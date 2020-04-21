library(readr)
library(dplyr)
library(tidyr)

# Read all raw datasets from UN into one dataframe ---------

df <- list.files(path='data/raw/UN Data/', full.names = TRUE) %>%
  lapply(read_delim, delim = ',', skip = 1) %>%
  bind_rows

# Remove columns that won't contribute to comparisons among countries
# or variables that we will deal with later.
df %>%
  select(-c(`Capital City`, `Capital City footnote`,
            `System of trade footnote`, `National currency`,
            `National currency footnote`, `Last Election Date footnote`,
            `Major trading partner 1 (% of exports) footnote`,
            `Tourism arrivals series type footnote`, Footnotes,
            `Region/Country/Area		Year	Series	Value	Footnotes	Source`,
            Source, `Major trading partner 1 (% of exports)`,
            `Tourism arrivals series type`, `Last Election Date`,
            `System of trade`)) -> df

# Bring it to a more usable format --------------------------------------------

# Rename columns
colnames(df) <- c('region_code', 'region_name', 'variable', 'year', 'value')

# Let's work on this dataframe now
df %>%
  # Keep only latest data for each variable
  group_by(variable) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  # Bring variables from rows to column
  pivot_wider(names_from = c(variable, year), values_from = value) -> df

# region_code was important for having a unique ID but now it's useless
df %>%
  select(-c(region_code)) -> df

# Let's add major trading partner (exportations)
# export
trading_partners <- read_delim('data/raw/UN Data/SYB62_330_201907_Major Trading Partners.csv',
                               skip = 1, delim=',')
trading_partners %>%
  group_by(Series) %>%
  filter(Year == max(Year)) %>%
  filter(Series %in% c('Major trading partner 1 (% of exports)')) %>%
  pivot_wider(names_from = Series,
              values_from = `Major trading partner 1 (% of exports)`) %>%
  ungroup() %>%
  select(-c(`Region/Country/Area`, Year,
            `Major trading partner 1 (% of exports) footnote`, Value,
            Footnotes, Source)) ->  trading_partners
colnames(trading_partners) <- c('region_name', 'whos_major_trade_partner_exp_1')
df <- left_join(df, trading_partners, by = 'region_name')
rm(trading_partners)

# Save UN dataset -------------------------------------

write_tsv(x = df, path = 'data/raw/UN_dataset.tsv', quote_escape = FALSE)
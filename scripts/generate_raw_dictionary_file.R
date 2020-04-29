library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(WriteXLS)

# Read all raw datasets from UN into one dataframe ---------

df <- fs::dir_ls('data/raw/UN Data/') %>%
  map_dfr(read_delim, delim = ',', skip = 1, .id = "filepath")

# Keep the variables required to create the data dictionary

df %>%
  select(Series, Year, Source, filepath) -> df

# Keep only latest data for each variable
df %>%
  group_by(Series) %>%
  filter(Year == max(Year)) %>%
  ungroup() -> df

# Variables are repeated for they occur for every country. Let's remove the
# duplicates.
df %>%
  distinct(Series, .keep_all = TRUE) -> df

# Fix names according to preprocessed dataset
df %>%
  # Make them all lowercase
  mutate(Series = tolower(Series)) %>%
  # Replace special chars
  mutate(Series = gsub(' ', '_', Series)) %>%
  mutate(Series = gsub('-', '_', Series)) %>%
  mutate(Series = gsub('_+', '_', Series)) %>%
  mutate(Series = gsub(',', '', Series)) -> df

# Add _year to the end of variable name, just like in the preprocessed file
df %>%
  unite("Series", Series:Year, remove=FALSE) -> df

# Add engineering UN variables
df <- rbind(df, c('whos_major_trade_partner_exp_1', 2018,
                  paste0('United Nations Statistics Division, New York, ',
                         'Commodity Trade Statistics Database (UN COMTRADE), ',
                         'last accessed May 2019.')))

# Add COVID-19 and engineering columns
df <- rbind(df,
            c('locality_code', '2020', paste0('European Centre for Disease ',
                                              'Prevention and Control. Last ',
                                              'accessed 28 April, 2020.')),
            c('locality_name', '2020', paste0('European Centre for Disease ',
                                              'Prevention and Control. Last ',
                                              'accessed 28 April, 2020.')),
            c('date', '2020', paste0('European Centre for Disease ',
                                     'Prevention and Control. Last ',
                                     'accessed 28 April, 2020.')),
            c('new_cases', '2020', paste0('European Centre for Disease ',
                                          'Prevention and Control. Last ',
                                          'accessed 28 April, 2020.')),
            c('new_deaths', '2020',  paste0('European Centre for Disease ',
                                            'Prevention and Control. Last ',
                                            'accessed 28 April, 2020.')),
            c('pop_data_2018', '2018', paste0('European Centre for Disease ',
                                              'Prevention and Control ',
                                              'collected from World Bank. ',
                                              'Last accessed 28 April, 2020.')),
            c('acc_cases', '2020', paste0('Engineered based on data from ',
                                          'European Centre for Disease ',
                                          'Prevention and Control. Last ',
                                          'accessed 28 April, 2020.')),
            c('acc_deaths', '2020', paste0('Engineered based on data from ',
                                           'European Centre for Disease ',
                                           'Prevention and Control. Last ',
                                           'accessed 28 April, 2020.')),
            c('lethality_rate_percent', '2020', paste0('Engineered based on data from ',
                                                       'European Centre for Disease ',
                                                       'Prevention and Control. Last ',
                                                       'accessed April, 2020.')),
            c('retail_recreation', '2020', paste0('Google Community Mobility ',
                                                  'Report. Last accessed 28 ',
                                                  'April, 2020')),
            c('grocery_pharmacy', '2020', paste0('Google Community Mobility ',
                                                 'Report. Last accessed 28 ',
                                                 'April, 2020')),
            c('parks', '2020', paste0('Google Community Mobility ',
                                      'Report. Last accessed 28 ',
                                      'April, 2020')),
            c('transit_stations', '2020', paste0('Google Community Mobility ',
                                                 'Report. Last accessed 28 ',
                                                 'April, 2020')),
            c('workplaces', '2020', paste0('Google Community Mobility ',
                                           'Report. Last accessed 28 ',
                                           'April, 2020')),
            c('residential', '2020', paste0('Google Community Mobility ',
                                            'Report. Last accessed 28 ',
                                            'April, 2020')),
            c('first_case_date', '2020',
              paste0('https://en.wikipedia.org/w/index.php?title=2019%E2%80%93',
                     '20_coronavirus_pandemic_by_country_and_territory&oldid=9',
                     '53662872 Last accessed April 28, 2020')),
            c('n_days_since_1st_case', '2020',
              paste0('Engineering from ECDC and https://en.wikipedia.org/w/index.php?title=2019%E2%80%93',
                     '20_coronavirus_pandemic_by_country_and_territory&oldid=9',
                     '53662872 Last accessed April 28, 2020')),
            c('first_death_date', '2020', paste0('Engineering based on ',
                                                 'data from ECDC ',
                                                 'counting the first death ',
                                                 'after February, 15th.')),
            c('n_days_since_1st_death', '2020', paste0('Engineering based on ',
                                                       'data from ECDC ',
                                                       'counting the first death ',
                                                       'after February, 15th.'))
            )
colnames(df) <- c('Variable name', 'Year', 'Source', 'Filepath')

# Add description for some variables
df$Description <- NA

df %>%
  mutate(Description = case_when(
    `Variable name` == 'retail_recreation' ~
      paste0('Mobility trends for places like restaurants, cafes, shopping ',
             'centers theme parks, museums, libraries, andmovie theaters.',
             'This variable indicates how visits and length of stay to this ',
             'category of location has varied (in percent, positively or ',
             'negatively) compared to the baseline. The baseline is the ',
             'median value, for the corresponding day of the week, during the ',
             '5-week period Jan 3–Feb 6, 2020'),
    `Variable name` == 'grocery_pharmacy' ~
      paste0('Mobility trends for places like grocery markets, ',
             'food warehouses, farmers markets, specialty food shops, drug ',
             'stores, and pharmacies.',
             'This variable indicates how visits and length of stay to this ',
             'category of location has varied (in percent, positively or ',
             'negatively) compared to the baseline. The baseline is the ',
             'median value, for the corresponding day of the week, during the ',
             '5-week period Jan 3–Feb 6, 2020'),
    `Variable name` == 'parks' ~
      paste0('Mobility trends for places like national parks, public beaches, ',
             'marinas, dog parks, plazas, and public gardens.',
             'This variable indicates how visits and length of stay to this ',
             'category of location has varied (in percent, positively or ',
             'negatively) compared to the baseline. The baseline is the ',
             'median value, for the corresponding day of the week, during the ',
             '5-week period Jan 3–Feb 6, 2020'),
    `Variable name` == 'transit_stations' ~
      paste0('Mobility trends for places like public transport hubs such as ',
             'subway, bus, and train stations.',
             'This variable indicates how visits and length of stay to this ',
             'category of location has varied (in percent, positively or ',
             'negatively) compared to the baseline. The baseline is the ',
             'median value, for the corresponding day of the week, during the ',
             '5-week period Jan 3–Feb 6, 2020'),
    `Variable name` == 'workplaces' ~
      paste0('Mobility trends for places of work.',
             'This variable indicates how visits and length of stay to this ',
             'category of location has varied (in percent, positively or ',
             'negatively) compared to the baseline. The baseline is the ',
             'median value, for the corresponding day of the week, during the ',
             '5-week period Jan 3–Feb 6, 2020'),
    `Variable name` == 'residential' ~
      paste0('Mobility trends for places of residence.',
             'This variable indicates how visits and length of stay to this ',
             'category of location has varied (in percent, positively or ',
             'negatively) compared to the baseline. The baseline is the ',
             'median value, for the corresponding day of the week, during the ',
             '5-week period Jan 3–Feb 6, 2020'),
    `Variable name` == 'date' ~
      paste0('Date for epidemiological variables. Format: YY-MM-DD'),
    `Variable name` == 'new_cases' ~
      paste0('Number of new cases for a specific date for a given country.'),
    `Variable name` == 'new_deaths' ~
      paste0('Number of new deaths for a specific date for a given country.'),
    `Variable name` == 'acc_cases' ~
      paste0('Accumulated number of cases up to the date for a given country.'),
    `Variable name` == 'acc_deaths' ~
      paste0('Accumulated number of deaths up to the date for a given ',
             'country.'),
    `Variable name` == 'lethality_rate_percent' ~
      paste0('Lethality rate in percent up to the last date in the dataset for',
             'a given country'),
    `Variable name` == 'first_case_date' ~
      paste0('The date of the first confirmed case of COVID-19 for a given ',
             'country. If it is NA it means that this country does not have ',
             'any death confirmed.'),
    `Variable name` == 'first_death_date' ~
      paste0('The date of the first confirmed death due to COVID-19 for a ',
             'given country, starting from February 15th, 2020. If it is NA',
             'it means that this country does not have any death confirmed.'),
    TRUE ~ '')) -> df

df$Filepath[c(174, 181:183, 190:193)] <- 'Does not apply. Engineered variable'
df$Filepath[c(175:180)] <- 'data/raw/COVID19_worldwide_raw.csv'
df$Filepath[c(184:189)] <- 'data/raw/Global_Mobility_Report.csv'

WriteXLS(x = df, ExcelFileName = 'data_dictionary.xls',
         SheetNames = 'Data Dictionary', BoldHeaderRow=TRUE)

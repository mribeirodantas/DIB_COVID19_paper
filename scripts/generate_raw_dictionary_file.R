library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(WriteXLS)

# Read all raw datasets from UN into one dataframe ---------

df <- fs::dir_ls('data/raw/UN Data/', glob='*.csv') %>%
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

df$Filepath[c(174, 180:182, 189:192)] <- 'Does not apply. Engineered variable'
df$Filepath[c(175:179)] <- 'data/raw/COVID19_worldwide_raw.csv'
df$Filepath[c(183:188)] <- 'data/raw/Global_Mobility_Report.csv'

# Add also docpath (PDF) to dictionary
df %>%
  mutate(Docpath = str_replace(Filepath, '.csv', '.pdf')) %>%
  mutate(Docpath = str_replace(Docpath, 'data/raw', 'documentation')) -> df

df$Docpath[175:192] <- 'There is no extra documentation.'

# Check documentation and see if variable names are correct ###
# According to the file below, maternal mortality rate is per 100k livebirths,
# not 100k people.
# SYB62_246_201907_Population growth and indicators of fertility and mortality.pdf
df %>%
  mutate(`Variable name` = ifelse(`Variable name` == paste0('maternal_mortal',
                                                            'ity_ratio_(deaths',
                                                            '_per_100000_popul',
                                                            'ation)_2015'),
                                  paste0('maternal_mortality_ratio_(deaths',
                                         '_per_100000_livebirths)_2015'),
                                  `Variable name`)) -> df

# According to the file below, it's participation rate.
# SYB62_329_201904_Labour Force and Unemployment.pdf
df %>%
  mutate(`Variable name` = ifelse(`Variable name` == paste0('labour_force_part',
                                                            'icipation_total_',
                                                            '2019'),
                                  paste0('labour_force_participation_rate_tot',
                                         'al_2019'),
                                  `Variable name`)) %>%
  mutate(`Variable name` = ifelse(`Variable name` == paste0('labour_force_part',
                                                            'icipation_male_',
                                                            '2019'),
                                  paste0('labour_force_participation_rate_mal',
                                         'e_2019'),
                                  `Variable name`)) %>%
  mutate(`Variable name` = ifelse(`Variable name` == paste0('labour_force_part',
                                                            'icipation_female',
                                                            '_2019'),
                                  paste0('labour_force_participation_rate_fema',
                                         'le_2019'),
                                  `Variable name`)) -> df


# Description
df %>%
  mutate(Description = case_when(
    `Variable name` == 'index_of_industrial_production:_total_industry_mining;_manufacturing;_electricity_gas_and_water_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_mining_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_manufacturing_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_food_beverages_and_tobacco_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_textiles_wearing_apparel_leather_footwear_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_chemicals_petroleum_rubber_and_plastic_products_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_metal_products_and_machinery_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_electricity_gas_steam_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_water_and_waste_management_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_basic_metals_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_electricity_gas_and_water_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_metal_products_(index_base:_2005=100)_2013' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_machinery_(index_base:_2005=100)_2013' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_miscellaneous_manufacturing_industries_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'index_of_industrial_production:_mining_and_manufacturing_(index_base:_2005=100)_2014' ~ 'The Index of Industrial Production measures volume changes in the production of an economy by International Standard Industrial Classification of All Economic Activities (ISIC)',
    `Variable name` == 'emissions_(thousand_metric_tons_of_carbon_dioxide)_2014' ~ 'Estimates are expressed in million metric tons of carbon. Per capita emission estimates are expressed in metric tons of carbon. Source: <https://cdiac.ess-dive.lbl.gov/ftp/ndp030/global.1751_2014.ems>.',
    `Variable name` == 'emissions_per_capita_(metric_tons_of_carbon_dioxide)_2014' ~ 'Per capita emission estimates are expressed in metric tons of carbon. Source: <https://cdiac.ess-dive.lbl.gov/ftp/ndp030/global.1751_2014.ems>.',
    `Variable name` == 'agricultural_production_(index_base:_2004_2006_=_100)_2016' ~ 'Relative level of the aggregate volume of agricultural production for each year in comparison with the base period 2004-2006',
    `Variable name` == 'food_production_(index_base:_2004_2006_=_100)_2016' ~ 'Food production index covers food crops that are considered edible and that contain nutrients. Coffee and tea are excluded because, although edible, they have no nutritive value. Source <https://data.worldbank.org/indicator/AG.PRD.FOOD.XD>.',
    `Variable name` == 'urban_population_(percent_growth_rate_per_annum)_2015' ~ 'Rate at which the percentage urban population grows or declines. Source: <https://population.un.org/wup/Publications/Files/WUP2018-Report.pdf>.',
    `Variable name` == 'rural_population_(percent_growth_rate_per_annum)_2015' ~ 'Rate at which the percentage rural population grows or declines. Source: <https://population.un.org/wup/Publications/Files/WUP2018-Report.pdf>.',
    `Variable name` == 'urban_population_(percent)_2018' ~ 'Urban Population percent at Mid-Year. Source: <https://population.un.org/wup/Publications/Files/WUP2018-Report.pdf>.',
    `Variable name` == 'capital_city_population_(as_a_percentage_of_total_population)_2015' ~ 'Capital city as a percentage of total population.',
    `Variable name` == 'capital_city_population_(as_a_percentage_of_total_urban_population)_2015' ~ 'Capital city as a percentage of total urban population.',
    `Variable name` == 'capital_city_population_(thousands)_2018' ~ 'Population living in capital city (thousand).',
    `Variable name` == 'surface_area_(thousand_km2)_2017' ~ 'Total surface area (square kilometres). Source: <https://unstats.un.org/unsd/demographic-social/products/dyb/dybsets/2015.pdf>',
    `Variable name` == 'population_mid_year_estimates_(millions)_2019' ~ 'De facto population in a country, area or region as of 1 July. Source: <https://population.un.org/wpp/Download/Standard/Population/>',
    `Variable name` == 'population_mid_year_estimates_for_males_(millions)_2019' ~ 'Male De facto population in a country, area or region as of 1 July. Source: <https://population.un.org/wpp/Download/Standard/Population/>',
    `Variable name` == 'population_mid_year_estimates_for_females_(millions)_2019' ~ 'Female De facto population in a country, area or region as of 1 July. Source: <https://population.un.org/wpp/Download/Standard/Population/>',
    `Variable name` == 'sex_ratio_(males_per_100_females)_2019' ~ 'Number of males per 100 females in the population. Source: <https://population.un.org/wpp/Download/Standard/Population/>',
    `Variable name` == 'population_aged_0_to_14_years_old_(percentage)_2019' ~ 'Percentage of population 0-14 aged group.',
    `Variable name` == 'population_aged_60+_years_old_(percentage)_2019' ~ 'Percentage of population 60+ aged group.',
    `Variable name` == 'population_density_2019' ~ 'Population per square Kilometre',
    `Variable name` == 'imports_cif_(millions_of_us_dollars)_2018' ~ 'The total value of imports (CIF) in millions of US dollars',
    `Variable name` == 'exports_fob_(millions_of_us_dollars)_2018' ~ 'The total value of exports (FOB) in millions of US dollars',
    `Variable name` == 'balance_imports/exports_(millions_of_us_dollars)_2018' ~ 'Balance of trade in millions of US dollars',
    `Variable name` == 'balance_of_payments:_current_account_(millions_of_us_dollars)_2018' ~ 'Current account of the balance of payments in millions of US dollars (Description on: https://data.imf.org/api/document/download?key=60567161)',
    `Variable name` == 'balance_of_payments:_capital_account_(millions_of_us_dollars)_2018' ~ 'Capital account of the balance of payments in millions of US dollars (Description on: https://data.imf.org/api/document/download?key=60567161)',
    `Variable name` == 'balance_of_payments:_financial_account_(millions_of_us_dollars)_2018' ~ 'Financial account of the balance of payments in millions of US dollars (Description on: https://data.imf.org/api/document/download?key=60567161)',
    `Variable name` == 'consumer_price_index:_general_2018' ~ 'General consumer price index',
    `Variable name` == 'consumer_price_index:_food_2018' ~ 'Consumer price index for food',
    `Variable name` == 'exchange_rates:_end_of_period_(national_currency_per_us_dollar)_2018' ~ 'National currency per US dollar, end of period',
    `Variable name` == 'exchange_rates:_period_average_(national_currency_per_us_dollar)_2018' ~ 'National currency per US dollar, period average ',
    `Variable name` == 'land_area_(thousand_hectares)_2016' ~ 'Country area (in thousand of hectares) excluding area under inland waters and coastal waters (as defined by FAO: http://www.fao.org/faostat/)',
    `Variable name` == 'arable_land_(thousand_hectares)_2016' ~ 'The total of areas (in thousand of hectares) under temporary crops, temporary meadows and pastures, and land with temporary fallow. Arable land does not include land that is potentially cultivable but is not normally cultivated.',
    `Variable name` == 'permanent_crops_(thousand_hectares)_2016' ~ 'Land cultivated (in thousand of hectares) with long-term crops which do not have to be replanted for several years, land under trees and shrubs producing flowers, and nurseries (except those for forest trees, which should be classified under ""Forestry""). Permanent meadows and pastures are excluded from land under permanent crops."',
    `Variable name` == 'forest_cover_(thousand_hectares)_2016' ~ 'Forest cover (in thousand of hectares) as defined by FAO (http://www.fao.org/faostat/)',
    `Variable name` == 'arable_land_(%_of_total_land_area)_2016' ~ 'The total of areas (in percentage) under temporary crops, temporary meadows and pastures, and land with temporary fallow. Arable land does not include land that is potentially cultivable but is not normally cultivated.',
    `Variable name` == 'permanent_crops_(%_of_total_land_area)_2016' ~ 'Land cultivated (in percentage) with long-term crops which do not have to be replanted for several years, land under trees and shrubs producing flowers, and nurseries (except those for forest trees, which should be classified under ""Forestry""). Permanent meadows and pastures are excluded from land under permanent crops."',
    `Variable name` == 'forest_cover_(%_of_total_land_area)_2016' ~ 'Forest cover (in percentage) as defined by FAO (http://www.fao.org/faostat/)',
    `Variable name` == 'important_sites_for_terrestrial_biodiversity_protected_(%_of_total_sites_protected)_2018' ~ 'Proportion of important sites for terrestrial biodiversity that are covered by protected areas (https://unstats.un.org/sdgs/metadata/files/Metadata-15-01-02.pdf)',
    `Variable name` == 'agriculture_hunting_forestry_and_fishing_(%_of_gross_value_added)_2016' ~ 'Gross value added of Agricultura, Hunting, Forestry and Fishing (in percentage)',
    `Variable name` == 'industry_(%_of_gross_value_added)_2016' ~ 'Gross value added of Industry (in percentage).',
    `Variable name` == 'services_(%_of_gross_value_added)_2016' ~ 'Gross value added of Services (in percentage).',
    `Variable name` == 'health_personnel:_physicians_(number)_2018' ~ 'Absolute number of physicians in a location.',
    `Variable name` == 'health_personnel:_physicians_(per_1000_population)_2018' ~ 'Rate of physicians for 1000 inhabitants (workforce density) in a location.',
    `Variable name` == 'health_personnel:_nurses_and_midwives_(number)_2018' ~ 'Absolute number of nurses and midwives in a location.',
    `Variable name` == 'health_personnel:_nurses_and_midwives_personnel_(per_1000_population)_2018' ~ 'Rate of nurses and midwives for 1000 inhabitants (workforce density) in a location.',
    `Variable name` == 'health_personnel:_dentists_(number)_2018' ~ 'Absolute number of dentists in a location.',
    `Variable name` == 'health_personnel:_dentists_(per_1000_population)_2018' ~ 'Rate of dentists for 1000 inhabitants (workforce density) in a location.',
    `Variable name` == 'health_personnel:_pharmacists_(number)_2018' ~ 'Absolute number of pharmacists in a location.',
    `Variable name` == 'health_personnel:_pharmacists_(per_1000_population)_2018' ~ 'Rate of pharmacists for 1000 inhabitants (workforce density) in a location.',
    `Variable name` == 'employment_by_industry:_agriculture_(%)_male_and_female_2019' ~ 'Percentage of total employment in agriculture, both sexes.',
    `Variable name` == 'employment_by_industry:_industry_(%)_male_and_female_2019' ~ 'Percentage of total employment in industry, both sexes.',
    `Variable name` == 'employment_by_industry:_services_(%)_male_and_female_2019' ~ 'Percentage of total employment in services, both sexes.',
    `Variable name` == 'employment_by_industry:_agriculture_(%)_male_2019' ~ 'Percentage of total employment in agriculture, males.',
    `Variable name` == 'employment_by_industry:_industry_(%)_male_2019' ~ 'Percentage of total employment in industry, males.',
    `Variable name` == 'employment_by_industry:_services_(%)_male_2019' ~ 'Percentage of total employment in service, males.',
    `Variable name` == 'employment_by_industry:_agriculture_(%)_female_2019' ~ 'Percentage of total employment in agriculture, females.',
    `Variable name` == 'employment_by_industry:_industry_(%)_female_2019' ~ 'Percentage of total employment in industry, females.',
    `Variable name` == 'employment_by_industry:_services_(%)_female_2019' ~ 'Percentage of total employment in service, females.',
    `Variable name` == 'net_official_development_assistance_received:_bilateral_(millions_of_us_dollars)_2017' ~ 'Bilateral aid (millions of US dollars).',
    `Variable name` == 'net_official_development_assistance_received:_multilateral_(millions_of_us_dollars)_2017' ~ 'Multilateral aid (millions of US dollars).',
    `Variable name` == 'net_official_development_assistance_received:_total_(millions_of_us_dollars)_2017' ~ 'Total aid (millions of US dollars).',
    `Variable name` == 'net_official_development_assistance_received:_total_(as_%_gni)_2017' ~ 'Total aid as a percentage of Gross National Income (GNI).',
    `Variable name` == 'gdp_in_current_prices_(millions_of_us_dollars)_2017' ~ 'In millions of US dollars at current prices.',
    `Variable name` == 'gdp_per_capita_(us_dollars)_2017' ~ 'per capita US dollars',
    `Variable name` == 'gdp_in_constant_2010_prices_(millions_of_us_dollars)_2017' ~ 'In millions of US dollars at constant 2010 prices.',
    `Variable name` == 'gdp_real_rates_of_growth_(percent)_2017' ~ 'Percentage of real rates of growth.',
    `Variable name` == 'current_expenditure_other_than_staff_compensation_as_%_of_total_expenditure_in_public_institutions_(%)_2018' ~ 'Current expenditure other than staff compensation as percentage of total expenditure in public education institutions,',
    `Variable name` == 'all_staff_compensation_as_%_of_total_expenditure_in_public_institutions_(%)_2018' ~ 'All staff compensation as percentage of total expenditure in public education institutions.',
    `Variable name` == 'capital_expenditure_as_%_of_total_expenditure_in_public_institutions_(%)_2018' ~ 'Capital expenditure as percentage of total expenditure in public education institutions.',
    `Variable name` == 'expenditure_by_level_of_education:_pre_primary_(as_%_of_government_expenditure)_2018' ~ 'Percentage of government expenditure on education for pre-primary education.',
    `Variable name` == 'expenditure_by_level_of_education:_primary_(as_%_of_government_expenditure)_2018' ~ 'Percentage of government expenditure on education for primary education.',
    `Variable name` == 'expenditure_by_level_of_education:_secondary_(as_%_of_government_expenditure)_2018' ~ 'Percentage of government expenditure on education for secondary education.',
    `Variable name` == 'expenditure_by_level_of_education:_tertiary_(as_%_of_government_expenditure)_2018' ~ 'Percentage of government expenditure on education for tertiary education.',
    `Variable name` == 'public_expenditure_on_education_(%_of_government_expenditure)_2018' ~ 'Government expenditure on education as percentage of government total expenditure.',
    `Variable name` == 'public_expenditure_on_education_(%_of_gdp)_2018' ~ 'Government expenditure on education as percentage of GDP. ',
    `Variable name` == 'population_annual_rate_of_increase_(percent)_2015' ~ 'Annual rate of growth of the population.',
    `Variable name` == 'infant_mortality_for_both_sexes_(per_1000_live_births)_2015' ~ 'Number of infant deaths (box sexes) per 1,000 births.',
    `Variable name` == 'maternal_mortality_ratio_(deaths_per_100000_livebirths)_2015' ~ 'Maternal deaths per 100000 livebirths',
    `Variable name` == 'life_expectancy_at_birth_for_both_sexes_(years)_2015' ~ 'Average number of years of life expected at age 0 for both sexes.',
    `Variable name` == 'total_fertility_rate_(children_per_women)_2016' ~ 'Average number of live births per woman.',
    `Variable name` == 'life_expectancy_at_birth_for_males_(years)_2018' ~ 'Average number of years of life expected at age 0, males.',
    `Variable name` == 'life_expectancy_at_birth_for_females_(years)_2018' ~ 'Average number of years of life expected at age 0, females.',
    `Variable name` == 'primary_energy_production_(petajoules)_2016' ~ 'Primary energy production in petajoules per capita.',
    `Variable name` == 'net_imports_[imports_exports_bunkers]_(petajoules)_2016' ~ 'Net imports [Imports - Exports - Bunkers] in petajoules (Production, trade and supply of energy).',
    `Variable name` == 'changes_in_stocks_(petajoules)_2016' ~ 'Changes in stocks in petajoules (Production, trade and supply of energy).',
    `Variable name` == 'total_supply_(petajoules)_2016' ~ 'Total supply of energy in petajoules per capita',
    `Variable name` == 'supply_per_capita_(gigajoules)_2016' ~ 'Supply of energy in gigajoules per capita.',
    `Variable name` == 'grants_of_patents_(number)_2017' ~ 'Number of grants of patents.',
    `Variable name` == 'patents_in_force_(number)_2017' ~ 'Number of patents in force.',
    `Variable name` == 'resident_patent_filings_(per_million_population)_2017' ~ 'Residents patent fillings per million people.',
    `Variable name` == 'r_&_d_personnel:_total_(number_in_full_time_equivalent)_2016' ~ 'Total number of people employed in R&D (Full-time equivalent - FTE).',
    `Variable name` == 'r_&_d_personnel:_researchers_total_(number_in_full_time_equivalent)_2016' ~ 'Total number of researchers (Full-time equivalent - FTE)',
    `Variable name` == 'r_&_d_personnel:_researchers_women_(number_in_full_time_equivalent)_2016' ~ 'Total number of female researchers (Full-time equivalent - FTE)',
    `Variable name` == 'r_&_d_personnel:_technicians_total_(number_in_full_time_equivalent)_2016' ~ 'Total number of technicians and equivalent staff (Full-time equivalent - FTE)',
    `Variable name` == 'r_&_d_personnel:_technicians_women_(number_in_full_time_equivalent)_2016' ~ 'Total number of female technicians and equivalent staff (Full-time equivalent - FTE)',
    `Variable name` == 'r_&_d_personnel:_other_supporting_staff_total_(number_in_full_time_equivalent)_2016' ~ 'Total number of other supporting staff in R&D (Full-time equivalent - FTE)',
    `Variable name` == 'r_&_d_personnel:_other_supporting_staff_women_(number_in_full_time_equivalent)_2016' ~ 'Total number of other female supporting staff in R&D (Full-time equivalent - FTE)',
    `Variable name` == 'gross_domestic_expenditure_on_r_&_d:_as_a_percentage_of_gdp_(%)_2016' ~ 'Gross domestic expenditure on R&D as percentage of GDP.',
    `Variable name` == 'gross_domestic_expenditure_on_r_&_d:_government_(%)_2016' ~ 'Percentage of expenditure on R&D coming from the government.',
    `Variable name` == 'gross_domestic_expenditure_on_r_&_d:_funds_from_abroad_(%)_2016' ~ 'Percentage of expenditure on R&D coming from funds from abroad.',
    `Variable name` == 'gross_domestic_expenditure_on_r_&_d:_not_distributed_(%)_2016' ~ 'Percentage of expenditure on R&D coming from non-specified source.',
    `Variable name` == 'gross_domestic_expenditure_on_r_&_d:_business_enterprises_(%)_2016' ~ 'Percentage of expenditure on R&D coming from business enterprises.',
    `Variable name` == 'gross_domestic_expenditure_on_r_&_d:_private_non_profit_(%)_2016' ~ 'Percentage of expenditure on R&D coming from private or non-profit institutiohns.',
    `Variable name` == 'gross_domestic_expenditure_on_r_&_d:_higher_education_(%)_2016' ~ 'Percentage of expenditure on R&D coming from Higher Education.',
    `Variable name` == 'gross_enrollment_ratio_tertiary_(female)_2017' ~ 'Gross enrollment ratio of female students at the tertiary level.',
    `Variable name` == 'students_enrolled_in_tertiary_education_(thousands)_2018' ~ 'Number of students enrolled (thousands) at the tertiary level.',
    `Variable name` == 'gross_enrollment_ratio_tertiary_(male)_2018' ~ 'Gross enrollment ratio of male students at the tertiary level.',
    `Variable name` == 'students_enrolled_in_primary_education_(thousands)_2018' ~ 'Number of students enrolled (thousands) at the primary level.',
    `Variable name` == 'students_enrolled_in_secondary_education_(thousands)_2018' ~ 'Number of students enrolled (thousands) at the secondary level.',
    `Variable name` == 'gross_enrollement_ratio_primary_(male)_2018' ~ 'Gross enrollment ratio of male students at the primary level.',
    `Variable name` == 'gross_enrollment_ratio_primary_(female)_2018' ~ 'Gross enrollment ratio of female students at the primary level.',
    `Variable name` == 'gross_enrollment_ratio_secondary_(male)_2018' ~ 'Gross enrollment ratio of male students at the secnodary level.',
    `Variable name` == 'gross_enrollment_ratio_secondary_(female)_2018' ~ 'Gross enrollment ratio of female students at the secondary level.',
    `Variable name` == 'threatened_species:_vertebrates_(number)_2019' ~ 'Number of threatened vertebrate species.',
    `Variable name` == 'threatened_species:_invertebrates_(number)_2019' ~ 'Number of threatened invertebrate species.',
    `Variable name` == 'threatened_species:_plants_(number)_2019' ~ 'Number of threatened plant species.',
    `Variable name` == 'threatened_species:_total_(number)_2019' ~ 'Number of total threatened species.',
    `Variable name` == 'percentage_of_individuals_using_the_internet_2017' ~ 'Percentage of the population using the internet.',
    `Variable name` == 'seats_held_by_women_in_national_parliament_as_of_february_(%)_2019' ~ 'Proportion (percentage) of seats held by women in national parliament as of February of 2019.',
    `Variable name` == 'ratio_of_girls_to_boys_in_primary_education_2017' ~ 'Ratio of girls to boys in primary education.',
    `Variable name` == 'ratio_of_girls_to_boys_in_secondary_education_2017' ~ 'Ratio of girls to boys in secondary education.',
    `Variable name` == 'ratio_of_girls_to_boys_in_tertiary_education_2017' ~ 'Ratio of girls to boys in tertiary education.',
    `Variable name` == 'teachers_at_tertiary_level_(thousands)_2018' ~ 'Number of teachers (thousands) at the tertiary level.',
    `Variable name` == 'pupil_teacher_ratio_in_tertiary_education_2018' ~ 'Pupil teacher ratio at the tertiary level.',
    `Variable name` == 'teachers_at_primary_level_(thousands)_2018' ~ 'Number of teachers (thousands) at the primary level.',
    `Variable name` == 'pupil_teacher_ratio_in_primary_education_2018' ~ 'Pupil teacher ratio at the primary level.',
    `Variable name` == 'teachers_at_secondary_level_(thousands)_2018' ~ 'Number of teachers (thousands) at the secondary level.',
    `Variable name` == 'pupil_teacher_ratio_in_secondary_education_2018' ~ 'Pupil teacher ratio at the secondary level.',
    `Variable name` == 'international_migrant_stock:_both_sexes_(number)_2017' ~ 'Estimates of international migrant (absolute number) based on official statistics on the foreign-born or the foreign population, for both sexes',
    `Variable name` == 'international_migrant_stock:_both_sexes_(%_total_population)_2017' ~ 'Estimates of international migrant (percent) based on official statistics on the foreign-born or the foreign population, for both sexes',
    `Variable name` == 'international_migrant_stock:_male_(%_total_population)_2017' ~ 'Estimates of international migrant (percent) based on official statistics on the foreign-born or the foreign population, males',
    `Variable name` == 'international_migrant_stock:_female_(%_total_population)_2017' ~ 'Estimates of international migrant (percent) based on official statistics on the foreign-born or the foreign population, females',
    `Variable name` == 'total_refugees_and_people_in_refugee_like_situations_(number)_2018' ~ 'Total number of refugees and people in refugee like situations (mid-year).',
    `Variable name` == 'asylum_seekers_including_pending_cases_(number)_2018' ~ 'Total number of asylum seekers including pending cases (mid-year).',
    `Variable name` == 'other_of_concern_to_unhcr_(number)_2018' ~ 'Total number of individuals in a refugee-like situation other than refugees/asylum seekers (mid-year).',
    `Variable name` == 'total_population_of_concern_to_unhcr_(number)_2018' ~ 'Total population of concern to UNHCR (mid-year).',
    `Variable name` == 'total_sexual_violence_at_the_national_level_rate_per_100000_2015' ~ 'Rate of sexual violence by 100000 inhabitants.',
    `Variable name` == 'intentional_homicide_rates_per_100000_2016' ~ 'Rate of intentional homicides by 100000 inhabitants.',
    `Variable name` == 'percentage_of_male_and_female_intentional_homicide_victims_male_2016' ~ 'Percentage of male victims to intentional homicides.',
    `Variable name` == 'percentage_of_male_and_female_intentional_homicide_victims_female_2016' ~ 'Percentage of female victims to intentional homicides.',
    `Variable name` == 'assault_rate_per_100000_population_2016' ~ 'Rate of assault by 100000 inhabitants.',
    `Variable name` == 'kidnapping_at_the_national_level_rate_per_100000_2016' ~ 'Rate of kidnappings by 100000 inhabitants.',
    `Variable name` == 'theft_at_the_national_level_rate_per_100000_population_2016' ~ 'Rate of theft by 100000 inhabitants.',
    `Variable name` == 'robbery_at_the_national_level_rate_per_100000_population_2016' ~ 'Rate of robbery by 100000 inhabitants.',
    `Variable name` == 'labour_force_participation_rate_total_2019' ~ 'Proportion of a country’s working-age population that engages actively in the labour market.',
    `Variable name` == 'unemployment_rate_total_2019' ~ 'Proportion of the labour force that does not have a job.',
    `Variable name` == 'labour_force_participation_rate_male_2019' ~ 'Proportion of a country’s working-age population (males) that engages actively in the labour market.',
    `Variable name` == 'unemployment_rate_male_2019' ~ 'Proportion of the labour force that does not have a job (males).',
    `Variable name` == 'labour_force_participation_rate_female_2019' ~ 'Proportion of a country’s working-age population (females) that engages actively in the labour market.',
    `Variable name` == 'unemployment_rate_female_2019' ~ 'Proportion of the labour force that does not have a job (females).',
    `Variable name` == 'major_trading_partner_1_(%_of_exports)_2018' ~ 'Percentage of total exports trade in US dollars with the major trading partner.',
    `Variable name` == 'major_trading_partner_1_(%_of_imports)_2018' ~ 'Percentage of total imports trade in US dollars with the major trading partner.',
    `Variable name` == 'major_trading_partner_2_(%_of_exports)_2018' ~ 'Percentage of total exports trade in US dollars with the second major trading partner.',
    `Variable name` == 'major_trading_partner_2_(%_of_imports)_2018' ~ 'Percentage of total imports trade in US dollars with the second major trading partner.',
    `Variable name` == 'major_trading_partner_3_(%_of_exports)_2018' ~ 'Percentage of total exports trade in US dollars with the third major trading partner.',
    `Variable name` == 'major_trading_partner_3_(%_of_imports)_2018' ~ 'Percentage of total imports trade in US dollars with the third major trading partner.',
    `Variable name` == 'tourism_expenditure_(millions_of_us_dollars)_2018' ~ 'Tourist/visitor expenditure (millions of US dollars).',
    `Variable name` == 'tourist/visitor_arrivals_(thousands)_2018' ~ 'Tourist/visitor arrivals (thousands).',
    `Variable name` == 'current_health_expenditure_(%_of_gdp)_2017' ~ 'Expenditure in health as percentage of GDP.',
    `Variable name` == 'domestic_general_government_health_expenditure_(%_of_total_government_expenditure)_2017' ~ 'Expenditure in health as percentage of government spending.',
    `Variable name` == 'whos_major_trade_partner_exp_1' ~ 'Engineering from variable Major trading partner 1 (% of exports), in the file SYB62_330_201907_Major Trading Partners.csv. Who is the country that is the major trading partner for exportation considering the percentage of total exports trade in US dollars. ',
    `Variable name` == 'locality_code' ~ 'ISO 3166-1 Alpha-2 is used for a 2 digit code for countries and regions. Hong Kong and Réunion are examples of regions.',
    `Variable name` == 'locality_name' ~ 'The name of the country or region. Hong Kong and Réunion are examples of regions.',
    `Variable name` == 'date' ~ 'Date for epidemiological variables. Format: YY-MM-DD',
    `Variable name` == 'new_cases' ~ 'Number of new cases for a specific date for a given country.',
    `Variable name` == 'new_deaths' ~ 'Number of new deaths for a specific date for a given country.',
    `Variable name` == 'acc_cases' ~ 'Accumulated number of cases up to the date for a given country.',
    `Variable name` == 'acc_deaths' ~ 'Accumulated number of deaths up to the date for a given country.',
    `Variable name` == 'lethality_rate_percent' ~ 'Lethality rate in percent up to the last date in the dataset fora given country',
    `Variable name` == 'retail_recreation' ~ 'Mobility trends for places like restaurants, cafes, shopping centers theme parks, museums, libraries, andmovie theaters.This variable indicates how visits and length of stay to this category of location has varied (in percent, positively or negatively) compared to the baseline. The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020',
    `Variable name` == 'grocery_pharmacy' ~ 'Mobility trends for places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies.This variable indicates how visits and length of stay to this category of location has varied (in percent, positively or negatively) compared to the baseline. The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020',
    `Variable name` == 'parks' ~ 'Mobility trends for places like national parks, public beaches, marinas, dog parks, plazas, and public gardens.This variable indicates how visits and length of stay to this category of location has varied (in percent, positively or negatively) compared to the baseline. The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020',
    `Variable name` == 'transit_stations' ~ 'Mobility trends for places like public transport hubs such as subway, bus, and train stations.This variable indicates how visits and length of stay to this category of location has varied (in percent, positively or negatively) compared to the baseline. The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020',
    `Variable name` == 'workplaces' ~ 'Mobility trends for places of work.This variable indicates how visits and length of stay to this category of location has varied (in percent, positively or negatively) compared to the baseline. The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020',
    `Variable name` == 'residential' ~ 'Mobility trends for places of residence.This variable indicates how visits and length of stay to this category of location has varied (in percent, positively or negatively) compared to the baseline. The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020',
    `Variable name` == 'first_case_date' ~ 'The date of the first confirmed case of COVID-19 for a given country.',
    `Variable name` == 'n_days_since_1st_case' ~ 'Number of days since the first case',
    `Variable name` == 'first_death_date' ~ 'The date of the first confirmed death due to COVID-19 for a given country, starting from February 15th, 2020.',
    `Variable name` == 'n_days_since_1st_death' ~ 'Number of days since the first death (counting from February 15th)',
    )
  ) -> df

# Change column ordering
df %>%
  select(`Variable name`, Year, Description, Source, Docpath, Filepath) -> df

WriteXLS(x = df, ExcelFileName = 'data_dictionary.xls',
         SheetNames = 'Data Dictionary', BoldHeaderRow=TRUE)

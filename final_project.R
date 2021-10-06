# Final Project in Data and Programming II course at the Harris School of Public Policy at the University of Chicago
# Winter Quarter 2021
# The project includes components on data wrangling and cleaning, visualizations, natural language processing, and data analysis
# Data sources include various economic indicators and migration data from Iran


library(tidyverse)
library(sf)
library(janitor)
library(readxl)
library(lubridate)
library(translateR)
library(htm2txt)
library(tidytext)
library(textstem)
library(wordcloud)
library(textdata)
library(scales)
library(shiny)

# !. DATA WRANGLING

# 1. un migration data. 
# imported in two stages and then merged to solve problem of "year" column turning into a row
migrant_a <- read_xlsx("un_total_migrant_stock.xlsx", range = "A15:F1997")
migrant_a <- migrant_a[-1,]
migrant_b <- read_xlsx("un_total_migrant_stock.xlsx", range = "G16:IG1997")

un_migrant_total <- bind_cols(c(migrant_a, migrant_b)) %>% 
  select(c(1, 3), "Iran (Islamic Republic of)") %>% 
  dplyr::rename("Count" = "Iran (Islamic Republic of)",
         "Destination" = "Major area, region, country or area of destination") %>% 
  filter(!is.na(Count),
         Count != "..") %>% 
  mutate(Year = as.integer(Year),
         Count = as.numeric(Count)) %>% 
  clean_names()

# 2. Maddison Project data
maddison <- read_xlsx("maddisonproject_mpd2020.xlsx",
                      sheet = "Full data",
                      col_names = TRUE) %>% 
  filter(country == "Iran (Islamic Republic of)",
         year >= 1950) %>% 
  select(-c(countrycode, country)) %>% 
  rename("real_gdp_percap_2011" = "gdppc",
         "population" = "pop") %>% 
  mutate(population = population * 1000)

# 3. gini index
gini <- read_xlsx("irandataportal_GINI-1982-to-2017.xlsx",
                  col_names = TRUE) %>% 
  filter(!is.na(`GINI Index`)) %>% 
  clean_names()
  
# 4. gdp growth 1960-2012: GDP Growth (constant prices 2004-2005)
gdp_growth <- read_xlsx("irandataportal_Iran-GDP-Growth-1960-2012.xlsx") %>% 
  clean_names() %>% 
  filter(!is.na(year_iranian_calendar)) %>% 
  mutate(year = str_replace(year, "\\d\\d\\d\\d-", ""),
         year = as.factor(year)) %>% 
  select(-year_iranian_calendar)
  
# 5. inflation
inflation <- read_xlsx("irandataportal_iran-inflation-f-.xlsx") %>% 
  clean_names() %>% 
  rename("year" = "year_western_calendar") %>% 
  filter(!is.na(year)) %>% 
  mutate(year = str_replace(year, "\\d\\d\\d\\d-", "")) %>% 
  select(-year_iranian_calendar)

# 6. gdp per capita (constant 2010 US$)
wb_gdp_per_cap <- read_csv("wb_gdp_real.csv", skip = 4) %>% 
  clean_names() %>% 
  filter(country_name == "Iran, Islamic Rep.") %>% 
  select(-country_code, -indicator_code)
for (col in 3:ncol(wb_gdp_per_cap)){
  colnames(wb_gdp_per_cap)[col] <-  sub("x", "", colnames(wb_gdp_per_cap)[col])
}
wb_gdp_per_cap <- wb_gdp_per_cap %>% 
  pivot_longer(`1960`:`2007`, names_to = "year", values_to = "gdp_per_capita_constant_2010_usd") %>% 
  select(-c(country_name, indicator_name), -c(`2008`:`2020`))

# combine datasets

library(plyr) # source: https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r
data <- join_all(list(gdp_growth,
              gini,
              inflation,
              maddison,
              wb_gdp_per_cap), by='year', type='full') %>% 
  mutate(year = as.integer(year)) %>% 
  arrange(year)

un_migrant_total_tidy <- un_migrant_total %>% 
  pivot_wider(names_from = destination, values_from = count)

final_df <- data %>% 
  left_join(un_migrant_total_tidy, by = "year")

write.csv(final_df, "~/Desktop/R/final-project-steven-gingold/final_dataframe.csv")
  

# PLOTTING

un_migrant_total %>% 
  filter(destination == "WORLD") %>% 
  ggplot(aes(year, count)) +
  geom_col(fill = "skyblue") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(x = "",
       y = "Migrants",
       title = "Total Iranian Migrants 1990-2019",
       caption = "Source: UN Population Division - International Migration")

un_migrant_total %>% 
  filter(destination == "Luxembourg") %>% 
  ggplot(aes(year, count)) +
  geom_col(fill = "navyblue") +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "Large Spike In Iranian Migration to Luxembourg",
       subtitle = "Maybe because Luxembourg is a tax haven?",
       caption = "Source: UN Population Division - International Migration")

ggplot(data, aes(x = year)) +
  geom_line(aes(y = gdp_growth, color = "gdp_growth"), na.rm = TRUE) +
  geom_line(aes(y = gdp_growth_excluding_oil, color = "gdp_growth_excluding_oil"), na.rm = TRUE) +
  scale_colour_manual("", 
                      breaks = c("gdp_growth", "gdp_growth_excluding_oil"),
                      values = c("red", "blue")) + 
  theme_minimal() +
  labs(x = "",
       y = "GDP Growth",
       title = "GDP Growth Over Time With and Without Oil Revenue",
       caption = "Source: Iran Data Portal - Syracuse University")

inflation %>% 
  mutate(year = as.Date(as.character(year), format = "%Y"),
         year = year(year),
         inflation_rate_percent = inflation_rate_percent / 100) %>% 
  ggplot(aes(year, inflation_rate_percent)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "",
       y = "",
       title = "Annual Inflation 1938-2015",
       caption = "Source: Iran Data Portal - Syracuse University")

wb_gdp_per_cap %>% 
  mutate(year = as.Date(as.character(year), format = "%Y"),
         year = year(year)) %>% 
  ggplot(aes(year, gdp_per_capita_constant_2010_usd)) +
  geom_line() +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "Iran's GDP Per Capita - Constant 2010 US$",
       caption = "Source: World Development Indicators")


# shiny 

# pivot data longer so all the indicators are in one column
data_long <- data %>% 
  pivot_longer(c(2:8), names_to = "indicator", values_to = "value")

ui <- fluidPage(
  selectInput(inputId = "Indicator",
              label = "Choose an Indicator",
              choices = data_long$indicator,
  plotOutput("line")   
)

server <- function(input, output) {
  plotData <- reactive({ 

    filter(data_long, indicator == input$Indicator)
  })
  output$line <- renderPlot({ 
    ggplot(plotData(), aes(x = year, y = value)) +
    geom_line() +
      theme_minimal() +
      labs(x = "Year",
           title = "Economic and Population Indicators in Iran")
  })
  
}

shinyApp(ui = ui, server = server)


# 3. TEXT PROCESSING

files <- lapply(Sys.glob("*.html"), gettxt) # source: https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files

my_texts <- list()
for (file in files) {
  my_texts[file] <- file %>% 
    str_remove("^.*?\\*") %>% 
    str_remove("B%.*")
}
names(my_texts) <- c("Aban", "Azar", "Bahman", "Dey", "Farvardin", "Khordad", "Mehr",
                     "Mordad", "Ordibehesht", "Shahrivar", "Tir")

pmi_reports <- tibble(month = names(my_texts), text = my_texts) 

# month names were ordered alphabetically, changing to chronological
pmi_reports$month <- as.factor(pmi_reports$month)
levels(pmi_reports$month) <- c("Farvardin", "Ordibehesht", "Khordad", "Tir", "Mordad", "Shahrivar", "Mehr",
            "Aban", "Azar", "Dey", "Bahman")

pmi_reports_words <-  unnest_tokens(pmi_reports, word_tokens, text, token = "words") %>% 
  anti_join(stop_words, by = c("word_tokens" = "word")) %>% 
  mutate(word_tokens = lemmatize_words(word_tokens)) 

# wordcloud! source: https://micah.waldste.in/blog/2017/10/introduction-to-sentiment-analysis-of-10-k-reports-in-r/
set.seed(90210)
pmi_wordcloud <- wordcloud(pmi_reports_words$word_tokens,
          scale = c(4, 0.5), 
          max.words = 40)

# source: https://github.com/sfeuerriegel/SentimentAnalysis
pmi_reports_words <- pmi_reports_words %>% 
  left_join(get_sentiments("loughran"), by = c("word_tokens" = "word")) %>%
  plyr::rename(replace = c(sentiment = "loughran", value = "loughran"), warn_missing = FALSE)

# plot using Iranian calendar
ggplot(data = filter(pmi_reports_words, !is.na(loughran))) +
  geom_histogram(aes(loughran), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(~ month) +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "Loughran Sentiment Analysis of 11 PMI Reports",
       subtitle = "Iranian Calendar - 1399",
       caption = "Source: Bourse & Bazaar")

# Change to Gregorian calendar
pmi_reports_greg <- pmi_reports_words %>% 
  mutate(month = case_when(
    month == "Aban" ~ "Oct 22-Nov 20",
    month == "Azar" ~ "Nov 21-Dec 20",
    month == "Bahman" ~ "Jan 20-Feb 18",
    month == "Dey" ~ "Dec 21-Jan 19",
    month == "Farvardin" ~ "Mar 20-Apr 19",
    month == "Khordad" ~ "May 21-June 20",
    month == "Mehr" ~ "Sep 22-Oct 21",
    month == "Mordad" ~ "July 22-Aug 21",
    month == "Ordibehesht" ~ "Apr 20-May 20",
    month == "Shahrivar" ~ "Aug 22-Sep 21",
    month == "Tir" ~ "June 21-July 21"
  )) %>% 
  mutate(month = as.factor(month))

levels(pmi_reports_greg$month) <- c("Mar 20-Apr 19",
                                  "Apr 20-May 20",
                                  "May 21-June 20",
                                  "June 21-July 21",
                                  "July 22-Aug 21",
                                  "Aug 22-Sep 21",
                                  "Sep 22-Oct 21",
                                  "Oct 22-Nov 20",
                                  "Nov 21-Dec 20",
                                  "Dec 21-Jan 19",
                                  "Jan 20-Feb 18")
        
ggplot(data = filter(pmi_reports_greg, !is.na(loughran))) +
  geom_histogram(aes(loughran), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(~ month) +
  theme_minimal() +
  labs(x = "",
       y = "",
       title = "Loughran Sentiment Analysis of 11 PMI Reports",
       subtitle = "Check out the negativity around the US election",
       caption = "Source: Bourse & Bazaar")


# ANALYSIS

# just a simple OLS model regressing Iranian migration totals on my economic indicators

model <- lm(WORLD ~ year + gdp_growth + gdp_growth_excluding_oil + gini_index + 
              inflation_rate_percent + real_gdp_percap_2011 + population, data = final_df)
summary(model)





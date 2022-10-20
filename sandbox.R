
summer_2018_raw <- read_csv("2018-summer-grades.csv")
summer_2018_detail <- read_csv("2018-summer-grades-detail.csv")
summer_2018 <- 
  summer_2018_raw %>% 
  left_join(summer_2018_detail, "CRN") %>% 
  mutate(year = 2018, term = "Summer") %>% 
  create_cols()

summer_2019_raw <- read_csv("2019-summer-grades.csv")
summer_2019_detail <- read_csv("2019-summer-grades-detail.csv")
summer_2019 <- 
  summer_2019_raw %>% 
  left_join(summer_2019_detail, "CRN") %>% 
  mutate(year = 2019, term = "Summer") %>% 
  create_cols()

summer_2020_raw <- read_csv("2020-summer-grades.csv")
summer_2020_detail <- read_csv("2020-summer-grades-detail.csv")
summer_2020 <- 
  summer_2020_raw %>% 
  left_join(summer_2020_detail, "CRN") %>% 
  mutate(year = 2020, term = "Summer") %>% 
  create_cols()

summer_2021_raw <- read_csv("2021-summer-grades.csv")
summer_2021_detail <- read_csv("2021-summer-grades-detail.csv")
summer_2021 <- 
  summer_2021_raw %>% 
  left_join(summer_2021_detail, "CRN") %>% 
  mutate(year = 2021, term = "Summer") %>% 
  create_cols()
# =========================
# DATA CLEANING PIPELINE
# =========================

library(dplyr)
library(readxl)
library(stringr)
library(janitor)
library(tidyr)

# --- Load Excel files ---
pop2 <- read_excel(
  "data_raw/PR - Rbootcamp - 841 rows.xlsx",
  skip = 1,
  col_names = FALSE
)

div2 <- read_excel(
  "data_raw/PR - Rbootcamp - divorces.xlsx",
  skip = 1,
  col_names = FALSE
)

# --- Population cleaning ---
pop <- pop2 %>%
  rename(
    year = ...1,
    year2 = ...2,
    canton_code = ...3,
    canton = ...4,
    poptype_code = ...5,
    poptype = ...6,
    sex_code = ...7,
    sex = ...8,
    marital_code = ...9,
    marital_status = ...10,
    population = ...11
  ) %>%
  mutate(across(c(year, year2), as.integer)) %>%
  tidyr::fill(
    year, year2, canton_code, canton,
    poptype_code, poptype, sex_code, sex,
    .direction = "down"
  )

pop_clean <- pop %>%
  filter(
    marital_status != "Marital status - total",
    sex %in% c("Male", "Female"),
    canton != "No indication"
  ) %>%
  select(year, canton, sex, marital_status, population)

# --- Divorce cleaning ---
div <- div2 %>%
  rename(
    year = ...1,
    year2 = ...2,
    canton_code = ...3,
    canton = ...4,
    duration_code = ...5,
    duration = ...6,
    divorces = ...7
  ) %>%
  mutate(across(c(year, year2), as.integer)) %>%
  tidyr::fill(year, year2, canton_code, canton, .direction = "down")

div_clean <- div %>%
  filter(duration != "Duration of marriage - total")

div_totals <- div_clean %>%
  group_by(year, canton) %>%
  summarise(divorces = sum(divorces), .groups = "drop")

# --- Join datasets ---
joined <- pop_clean %>%
  left_join(div_totals, by = c("year", "canton"))

# --- Save outputs ---
write.csv(pop_clean, "data_clean/pop_clean.csv", row.names = FALSE)
write.csv(div_totals, "data_clean/div_totals.csv", row.names = FALSE)
write.csv(joined, "data_clean/joined_pop_div.csv", row.names = FALSE)

saveRDS(pop_clean, "data_clean/pop_clean.rds")
saveRDS(div_totals, "data_clean/div_totals.rds")
saveRDS(joined, "data_clean/joined_pop_div.rds")

cat("Cleaning complete ✅\n")

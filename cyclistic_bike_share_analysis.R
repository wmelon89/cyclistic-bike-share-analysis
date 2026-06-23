# =========================
# STEP 1: Load Required Libraries
# =========================
library(tidyverse)
library(lubridate)


# =========================
# STEP 2: Set Working Directory
# =========================
home <- Sys.getenv("USERPROFILE")

setwd(file.path(
  home,
  "Documents",
  "Google Data Analytics Certificate",
  "Capstone-Project"
))


# =========================
# STEP 3: Verify the 12 Monthly CSV Files
# =========================
data_dir <- file.path("FY2025-Bike-Share-Data", "Original")

files <- list.files(
  data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

length(files)
sort(basename(files))

# Ensure that all 12 monthly files are present
stopifnot(length(files) == 12)


# =========================
# STEP 4: Import and Combine All CSV Files
# =========================
raw <- map_dfr(
  files,
  read_csv,
  show_col_types = FALSE
)


# =========================
# STEP 5: Create New Variables and Clean Data
# =========================
trips <- raw %>%
  mutate(
    ride_length = as.numeric(
      difftime(ended_at, started_at, units = "mins")
    ),
    date = as.Date(started_at),
    month = month(started_at, label = TRUE, abbr = FALSE,locale = "C"),
    day_of_week = wday(
      started_at,
      label = TRUE,
      abbr = FALSE,
      week_start = 7,
      locale = "C"
    ),
    year = year(started_at)
  ) %>%
  filter(
    ride_length > 1,
    ride_length < 24 * 60
  )


# =========================
# STEP 6: Perform Initial Data Exploration
# =========================
summary(trips$ride_length)
table(trips$month)
table(trips$day_of_week)


# =========================
# STEP 7: Analyze Usage by Rider Type
# =========================
by_member <- trips %>%
  group_by(member_casual) %>%
  summarise(
    rides = n(),
    avg_duration = mean(ride_length),
    median_duration = median(ride_length),
    .groups = "drop"
  )

by_member


# =========================
# STEP 8: Analyze Usage by Day of Week
# =========================
weekday_member <- trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    rides = n(),
    avg_duration = mean(ride_length),
    .groups = "drop"
  )

weekday_member


# =========================
# STEP 9: Analyze Usage by Month
# =========================
month_member <- trips %>%
  group_by(member_casual, month) %>%
  summarise(
    rides = n(),
    avg_duration = mean(ride_length),
    .groups = "drop"
  )

month_member

# =========================
# STEP 10: Create Monthly Ride Count Visualization
# =========================

ggplot(
  month_member,
  aes(
    x = month,
    y = rides,
    color = member_casual,
    group = member_casual
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Rides by User Type",
    x = "Month",
    y = "Number of Rides",
    color = "User Type"
  )+
  scale_y_continuous(labels = scales::comma)

# =========================
# STEP 11: Create Average Ride Duration Visualization
# =========================

ggplot(
  month_member,
  aes(
    x = month,
    y = avg_duration,
    color = member_casual,
    group = member_casual
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Average Ride Duration by User Type",
    x = "Month",
    y = "Average Duration (Minutes)",
    color = "User Type"
  )

# =========================
# STEP 12: Create Weekly Average Ride Duration Visualization
# =========================

ggplot(
  weekday_member,
  aes(
    x = day_of_week,
    y = rides,
    color = member_casual,
    group = member_casual
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Weekly Rides by User Type",
    x = "Weekday",
    y = "Number of Rides",
    color = "User Type"
  ) +
  scale_y_continuous(labels = scales::comma)
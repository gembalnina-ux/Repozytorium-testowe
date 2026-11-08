dane <- dane %>%
  mutate(
    Release = as.Date(Release.Date, format = "%m/%d/%Y"),
    Netflix.Release = as.Date(Netflix.Release.Date, format = "%m/%d/%Y")
  )

dane %>%
  filter(grepl("Woody Allen", Director),
         !is.na(Release)) %>%
  arrange(Release) %>%
  select(Title, Release) %>%
  head(1)
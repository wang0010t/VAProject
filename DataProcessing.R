#For processing large datasets by filtering out only necessary data and saving the filtered data into rds file

checkin_journal <- read_csv("data/CheckinJournal.csv")
checkin_journal$timestamp <- as.Date(checkin_journal$timestamp, "%Y-%m-%d")
checkin_journal_selected <- checkin_journal%>%
  filter(`venueType` == c("Pub", "Restaurant", "Workplace"))
write_rds(checkin_journal_selected, "data/checkin_journal_selected.rds")

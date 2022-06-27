#For processing large datasets by filtering out only necessary data and saving the filtered data into rds file

checkin_journal <- read_csv("data/CheckinJournal.csv")
checkin_journal$timestamp <- as.Date(checkin_journal$timestamp, "%Y-%m-%d")
checkin_journal_selected <- checkin_journal%>%
  filter(`venueType` == c("Pub", "Restaurant", "Workplace"))
write_rds(checkin_journal_selected, "data/checkin_journal_selected.rds")

travel_journal <- read_csv("data/NOT UPLOADED/TravelJournal.csv")
pubs <- read_csv("data/Pubs.csv")
restaurants <- read_csv("data/Restaurants.csv")
pub_cust_rev <- travel_journal%>%
  filter (travelEndLocationId %in% pull(pubs, pubId))%>%
  filter (purpose %in% c("Recreation (Social Gathering)","Eating"))%>%
  mutate(spent = startingBalance - endingBalance,
         month = month(checkInTime),
         year = year(checkInTime),
         travelEndLocationId=as.character(travelEndLocationId))%>%
  group_by(travelEndLocationId, month, year)%>%
  summarize(customers = n(), revenue = sum(spent))%>%
  mutate(monthYear = format(as.Date(paste(`year`, `month`, "1", sep = "-"), format = "%Y-%m-%d"), "%Y-%m"),
         RevenuePerCustomer = revenue/customers)%>%
  rename(venueId = travelEndLocationId)%>%
  ungroup()
glimpse(pub_cust_rev)
write_rds(pub_cust_rev, "data/pub_cust_rev.rds")

rest_cust_rev <- travel_journal%>%
  filter (travelEndLocationId %in% pull(restaurants, restaurantId))%>%
  filter (purpose %in% c("Recreation (Social Gathering)","Eating"))%>%
  mutate(spent = startingBalance - endingBalance,
         month = month(checkInTime),
         year = year(checkInTime),
         travelEndLocationId=as.character(travelEndLocationId))%>%
  group_by(travelEndLocationId, month, year)%>%
  summarize(customers = n(), revenue = sum(spent))%>%
  mutate(monthYear = format(as.Date(paste(`year`, `month`, "1", sep = "-"), format = "%Y-%m-%d"), "%Y-%m"),
         RevenuePerCustomer = revenue/customers)%>%
  rename(venueId = travelEndLocationId)%>%
  ungroup()
glimpse(rest_cust_rev)
write_rds(rest_cust_rev, "data/rest_cust_rev.rds")

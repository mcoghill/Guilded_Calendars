# Get Guilded calendar events (only works on public calendars)
library(jsonlite)
library(git2r)

# Connect to git???
config(user.name = "mcoghill", user.email = "matthewcoghill@live.ca")

# Step 1: Scrape the calendar for events up to a year ahead
start_day <- "2021-01-01T00:00:00.000Z"
end_day <- paste0(Sys.Date() + 365, "T00:00:00.000Z")
event_url <- "https://www.guilded.gg/api/channels/3dbd7b6f-c855-4056-9d9c-200908b3c80e/events"
calendar_url <- paste0(event_url, "?startDate=", start_day, "&endDate=", end_day)

# Step 2: Download the list of calendar events as json
events <- fromJSON(calendar_url)
event_ids <- events$events$id

# Step 3: Download individual events as .ics files
dl_dir <- file.path("./temp")
dir.create(dl_dir, showWarnings = FALSE)

ics_dl <- lapply(event_ids, function(x) {
  download.file(paste0(event_url, "/", x, "?format=ics"), file.path(dl_dir, paste0(x, ".ics")), mode = "wb")
  y <- readLines(file.path(dl_dir, paste0(x, ".ics")))
  y <- c(y[-c(1:3, length(y))], "\n")
  return(y)
})

# Finalize file output
ics_comb <- c("BEGIN:VCALENDAR", "VERSION:2.0", "CALSCALE:GREGORIAN", unlist(ics_dl))
ics_comb <- c(ics_comb[-length(ics_comb)], "END:VCALENDAR")
writeLines(ics_comb, "new_calendar.ics")

# Now, figure out a way to update the file to GitHub on a given frequency
# Need to compare the new file to the one on GitHub currently - if there is a change,
# push changes; if not, leave it alone. Check every ~1 minute???


add(path = "new_calendar.ics")
commit(message = "")

# Get Guilded calendar events (only works on public calendars)
library(tidyverse)
library(jsonlite)
library(data.table)
library(gert)

# Download modified calendar package
if(!"calendar" %in% installed.packages()[, "Package"]) {
  devtools::install_github("mcoghill/calendar")
}
library(calendar)

# Step 1: Scrape the calendar for events up to a year ahead
# Calendar events are listed in groups of 25 max, so iterate by month starting
# in October 2021

# Run in a continuous loop
running <- TRUE

while(running) {
  n_months <- ceiling((Sys.Date() + 365 - as.Date("2021-10-01")) / (365/12))
  start_days <- paste0(seq(as.Date("2021-10-01"), length = n_months, by = "months"), "T00:00:00.000Z")
  end_days <- paste0(seq(as.Date("2021-11-01"), length = n_months, by = "months") - 1, "T00:00:00.000Z")
  event_url <- "https://www.guilded.gg/api/channels/3dbd7b6f-c855-4056-9d9c-200908b3c80e/events"
  calendar_urls <- sapply(1:length(start_days), function(x) {
    paste0(event_url, "?startDate=", start_days[x], "&endDate=", end_days[x])
  }, simplify = TRUE)
  
  # Step 2: Download the list of calendar events as json
  event_ids <- unique(unlist(lapply(calendar_urls, function(x) {
    y <- fromJSON(x)
    if(length(y$events)) { 
      return(y$events$id)
    } else NULL
  })))
  
  # Step 3: Download individual events as .ics files (faster in parallel)
  dl_dir <- file.path("./temp")
  dir.create(dl_dir, showWarnings = FALSE)
  
  ics_dl <- lapply(event_ids, function(x) {
    if(!file.exists(file.path(dl_dir, paste0(x, ".ics")))) {
      download.file(paste0(event_url, "/", x, "?format=ics"), 
                    file.path(dl_dir, paste0(x, ".ics")), quiet = TRUE, mode = "wb")
    }
    y <- ic_read(file.path(dl_dir, paste0(x, ".ics")))
    return(y)
  }) |> rbindlist(fill = TRUE) |> 
    ical(ic_attributes = ic_attributes_vec(readLines(file.path(dl_dir, paste0(event_ids[1], ".ics")))))
  
  ic_write(ics_dl, "calendar.ics")
  
  # Now, figure out a way to update the file to GitHub on a given frequency
  # Need to compare the new file to the one on GitHub currently - if there is a change,
  # push changes; if not, leave it alone. Check every ~1 minute???
  
  # Download "old" calendar
  download.file("https://raw.githubusercontent.com/mcoghill/Guilded_Calendars/main/calendar.ics", 
                "old_calendar.ics", quiet = TRUE)
  old_cal <- ic_read("old_calendar.ics")
  new_cal <- ic_read("calendar.ics")
  
  # Check UID's for new calendar events
  if(!all(new_cal$UID %in% old_cal$UID) & all(old_cal$UID %in% new_cal$UID)) {
    git_add("calendar.ics")
    git_commit(message = "ics upload")
    git_push()
    invisible(gc())
    Sys.sleep(600)
  } else {
    invisible(gc())
    Sys.sleep(600)
  }
}

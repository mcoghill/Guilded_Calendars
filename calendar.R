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

# Run in a continuous loop, update on scheduled frequency (in seconds)
running <- TRUE
update_freq <- 300

while(running) {
  # Step 1: Scrape the calendar for events up to a year ahead
  # Calendar events are listed in groups of 25 max, so iterate by month starting
  # in October 2021
  
  # Generate sequence of start and end dates, paste into URL
  n_months <- as.numeric(ceiling((Sys.Date() + 365 - as.Date("2021-10-01")) / (365/12)))
  start_days <- paste0(seq(as.Date("2021-10-01"), length = n_months, by = "months"), "T00:00:00.000Z")
  end_days <- paste0(seq(as.Date("2021-11-01"), length = n_months, by = "months") - 1, "T24:00:00.000Z")
  event_url <- "https://www.guilded.gg/api/channels/3dbd7b6f-c855-4056-9d9c-200908b3c80e/events"
  calendar_urls <- sapply(1:length(start_days), function(x) {
    paste0(event_url, "?startDate=", start_days[x], "&endDate=", end_days[x])
  }, simplify = TRUE)
  
  # Step 2: Download the list of calendar events as json to get event ID's
  # (faster in parallel)
  events <- lapply(calendar_urls, fromJSON) |>
    lapply("[[", 1) |>
    bind_rows() |>
    dplyr::select(!where(is.list)) |>
    distinct() |>
    dplyr::filter(!isCancelled)
  
  event_ids <- events$id
  
  # Step 3: Download individual events as .ics files if they aren't already
  # downloaded (faster in parallel)
  dl_dir <- file.path("./temp")
  dir.create(dl_dir, showWarnings = FALSE)
  
  ics_dl <- lapply(event_ids, function(x) {
    if(!file.exists(file.path(dl_dir, paste0(x, ".ics")))) {
      download.file(paste0(event_url, "/", x, "?format=ics"), 
                    file.path(dl_dir, paste0(x, ".ics")), quiet = TRUE, mode = "wb")
    }
    y <- ic_read(file.path(dl_dir, paste0(x, ".ics")))
    
    # Fix for the event times using base R
    if(all(c("DTSTART", "DTEND") %in% names(y))) {
      y_sdiff <- as.numeric(format(y$DTSTART, "%z")) / 100 * 60 * 60
      y_ediff <- as.numeric(format(y$DTEND, "%z")) / 100 * 60 * 60
      y$DTSTART <- y$DTSTART + y_sdiff
      y$DTEND <- y$DTEND + y_ediff
    }
    return(y)
  }) |> rbindlist(fill = TRUE) |> 
    ical(ic_attributes = ic_attributes_vec(readLines(file.path(dl_dir, paste0(event_ids[1], ".ics")))))
  
  ic_write(ics_dl, "calendar.ics")
  
  # Now, figure out a way to update the file to GitHub on a given frequency
  # Need to compare the new file to the one on GitHub currently - if there is a change,
  # push changes; if not, leave it alone. Check every ~1 minute???
  
  # Load in the "old" calendar. If it doesn't currently exist on the hard drive,
  # try downloading it from the GitHub link. If the file doesn't exist on GitHub,
  # post the new file to the GitHub page and write that new file as the "old" 
  # calendar. No matter what, there should be an "old" calendar on disk now.
  if(!file.exists("old_calendar.ics")) {
    try(download.file("https://raw.githubusercontent.com/mcoghill/Guilded_Calendars/main/calendar.ics", 
                  "old_calendar.ics", quiet = TRUE), silent = TRUE)
    if(!file.exists("old_calendar.ics")) {
      git_add("calendar.ics")
      git_commit(message = "ics upload")
      git_push()
      ic_write(ics_dl, "old_calendar.ics")
    }
  }
  
  # Read in the old and new calendars
  old_cal <- ic_read("old_calendar.ics")
  new_cal <- ic_read("calendar.ics")
  
  # Check for identical tables before uploading new one; disregard the "DTSTAMP" columns
  if(!identical(new_cal[, -which(names(new_cal) == "DTSTAMP")], old_cal[, -which(names(old_cal) == "DTSTAMP")])) {
    git_add("calendar.ics")
    git_commit(message = "ics upload")
    git_push()
    ic_write(ics_dl, "old_calendar.ics")
  } 
  invisible(gc())
  Sys.sleep(update_freq)
}

library(readr)
library(lubridate)
library(kableExtra)
library(huxtable)
library(dplyr)
options(knitr.kable.NA = "")

############################################# functions ###########################################
# function for generate outline table
generateOutlineTable <- function(outline) {
  # split by date
  ol_tb <- split(outline, f = outline$DATE) |> lapply(
    # for each date
    function(ol_daily) {
      tz <- names(which(tz_dict == attr(ol_daily$FROM, "tzone")[1])) # current timezone
      content_rows <- which(ol_daily$TYPE == "Content")              # talks
      session_rows <- which(ol_daily$TYPE == "Session")              # session names
      keynote_rows <- grep("Keynote", ol_daily$TOPIC)                # keynote rows
      parallel_rows <- grep("Breakout Sessions", ol_daily$TOPIC) # parallel session name
      ol_date <- format(ol_daily$DATE[1], "%a, %B %d")               # date used as tab name 
      if (length(content_rows)) {
        ol_daily[content_rows, c("DATE", "FROM", "TO")] <- NA        # clean up for talk rows
      }
      ol_daily$FROM <- format(ol_daily$FROM, "%I:%M %p")             # format display time
      ol_daily$TO <- format(ol_daily$TO, "%I:%M %p")                 # format display time
      ol_daily$SPAN <- NA                                            # display time in FROM - TO format
      ol_daily$SPAN[session_rows] <- paste0(ol_daily[session_rows, ]$FROM, " -- ", ol_daily[session_rows, ]$TO)
      ol_daily[, c("FROM", "TO", "TYPE", "DATE")] <- NULL            # clean up table
      if (length(keynote_rows)) {           # create link to keynote session in full program
        ol_daily[keynote_rows, ]$TOPIC <- paste0("[", ol_daily[keynote_rows, ]$TOPIC, "](#", paste0(tz, "-", ol_daily[keynote_rows, ]$SESSION, "-keynote"), ")")
      }
      if (length(parallel_rows)) {          # create link to parallel session head in full program
        ol_daily[parallel_rows, ]$TOPIC <- paste0("[", ol_daily[parallel_rows, ]$TOPIC, "](#", paste0(tz, "-", ol_daily[parallel_rows, ]$SESSION, "-parallel"), ")")
      }
      
      ol_daily <- ol_daily[, c("SPAN", "LOCATION", "TOPIC")]  # clean up table
      outline_table <- knitr::kable(ol_daily, "html", col.names = NULL) %>%   
        kable_styling(full_width = TRUE, font_size = 14, bootstrap_options = "hover", position = "left") %>%
        column_spec(1, width = "30%") # build table
      if (length(content_rows)) {      
        outline_table <- kableExtra::add_indent(outline_table, content_rows, level_of_indent = 2, all_cols = TRUE)
      } # indent talk rows
      return(
        list(
          ol_date = ol_date,
          ol_table = outline_table
        )
      )
    }
  )
}

# generate the content under each topic
generateEachTopic <- function(t) {
  # topic table head: topic + chair
  df_head <- data.frame(
    "C4" = paste0("**", t$TOPIC[1], "**"), # topic
    "C5" = NA,
    "C6" = ifelse(!is.na(t$CHAIR_CONFIRMED[1]),  paste0("Chair: ", t$CHAIR_CL[1], ""), NA) # chair
  )
  # talks in topic table 
  SPEAKERS <- cbind(t$SPEAKER1, t$SPEAKER2)
  SPEAKERS <- apply(SPEAKERS, 1, function(x) paste(na.omit(x),collapse=", "))
  df_content <- data.frame(
    "C4" = paste0(format(t$FROM, "%I:%M %p"), "--", format(t$TO, "%I:%M %p")),  # display time
    "C5" = ifelse((t$TITLE != "(Withdrawn)"), SPEAKERS,  NA), # speaker
    "C6" = ifelse((t$TITLE != "(Withdrawn)") & (!grepl("Invited", t$TYPE)), paste0("[", t$TITLE, "](#abstract-", t$ID, ")"), t$TITLE)
    # talk title
  )
  df <- rbind(df_head, df_content)
  # df["C3"] <- t$SESSION_NUM[1] # session number
  df["C3"] <- t$LOCATION[1] # display location info
  df["C7"] <- "para" # to determine if subtable for parallel sessions
  df[c("C0", "C1", "C2")] <- NA
  df <- df[, c("C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7")]
  return(df)
}

# generate full program tables by session
generateSessionTable <- function(full_prog) {
  session_order <- unique(full_prog$SESSION)
  # split by session
  sessions <- split(full_prog, f = full_prog$SESSION) |> lapply(
    function(ss) {
      # parallel sessions in session
      para_rows <- grep("Parallel Session|Invited Session", ss$TYPE)
      # keynote, dialogue, networking, etc...
      nonpara_rows <- grep("Parallel|Invited Session|Keynote|Dialogue Session", ss$TYPE, invert = TRUE)
      keynote_rows <- grep("Keynote|Dialogue Session", ss$TYPE)
      # date to display in full program table
      ss$DATE <- paste0('<p class = "fp-date">',  format(ss$DATE, "%b %d"), '</p>')
      # aggregate time span for each entire session
      ss$AGG_SPAN <- paste0(format(ss$FROM, "%I:%M %p"), "--", format(ss$TO, "%I:%M %p"))
      # get current timezone, needed for building the anchor
      tz <- names(which(tz_dict == attr(ss$FROM, "tzone")[1]))
      if(length(para_rows)){
        # for parallel sessions, the aggregate time span is from the smallest FROM to the largest TO
        ss[para_rows, ]$AGG_SPAN <- paste(format(min(ss$FROM[para_rows]), "%I:%M %p"), "--", format(max(ss$TO[para_rows]), "%I:%M %p"))
        # parallel session head
        para_table_head <- data.frame(
          "C0" = ss$FROM[para_rows[1]], # for sorting
          "C1" = ss$DATE[para_rows[1]], # date for display
          "C2" = ss$AGG_SPAN[para_rows[1]], # aggregate time span in column 2
          "C3" = NA,
          # parallel session topic with anchor
          "C4" = paste0('**<a class="target-pos" name = "', paste0(tz, "-", ss$SESSION[1], "-parallel"), '">', ss$TYPE[para_rows[1]], "</a>**"),
          "C5" = NA,
          "C6" = NA,
          "C7" = NA
        )
        # generate each topic within each session
        para_by_session <- split(ss[para_rows, ], f = ss[para_rows, ]$SESSION_NUM) |> lapply(
          function(s) {
            # split topic by topic name 
            t <- split(s, f = paste0(s$TOPIC)) |> lapply(generateEachTopic) |> (\(x) do.call("rbind", x))()
            return(t)
          }
        ) |> (\(x) do.call("rbind", x))()
        # parallel session table
        para_table <- rbind(para_table_head, para_by_session)
        para_table$C0 <- para_table$C0[1] # for sorting
      } else {
        para_table <- NULL
      }
      
      # combine parallel session with any other nonparallel session 
      if (length(nonpara_rows)) {
        nonpara_table <- data.frame(
          "C0" = ss$FROM[nonpara_rows],
          "C1" = ss$DATE[nonpara_rows],
          "C2" = ss$AGG_SPAN[nonpara_rows],
          "C3" = ss$LOCATION[nonpara_rows],
          "C4" = paste0("**", ss$TYPE[nonpara_rows], "**"),
          "C5" = ss$SPEAKER[nonpara_rows],
          "C6" = ss$TITLE[nonpara_rows],
          "C7" = NA
        )
      } else {
         nonpara_table <- NULL
      }
      
      if (length(keynote_rows)){
        keynote_table <- data.frame(
          "C0" = ss$FROM[keynote_rows],
          "C1" = ss$DATE[keynote_rows],
          "C2" = ss$AGG_SPAN[keynote_rows],
          "C3" = ss$LOCATION[keynote_rows],
          "C4" = paste0("**", ss$TYPE[keynote_rows], "**\n\r", "Chair: ", ss$CHAIR_CL[keynote_rows]),
          "C5" = ss$SPEAKER[keynote_rows],
          "C6" = paste0("[", ss[keynote_rows, ]$TITLE, "](#abstract-", ss$ID[keynote_rows], ")"),
          "C7" = NA
        )
      } else {
        keynote_table <- NULL
      }
      ss_table <- rbind(nonpara_table, para_table, keynote_table)
      ss_table <- ss_table[order(ss_table$C0, decreasing = FALSE), ] # sort sessions by time
      ss_table["C0"] <- NULL  # no longer needed
      rownames(ss_table) <- NULL
      # build table
      ss_table <- as_huxtable(ss_table)
      ss_table <- ss_table[-1, ]
      long_title_idx <- which(is.na(ss_table$C5))
      for (i in long_title_idx) {
        ss_table <- ss_table %>% merge_cells(i, 4:5) %>% set_escape_contents(i, 6, FALSE)
      }
      session_numbers <- na.omit(unique(ss_table[ss_table$C7 == "para",]$C3))
      if(length(session_numbers)){
        for (sn in session_numbers) {
          s_rows <- which(ss_table$C3 == sn & ss_table$C7 == "para")
          s_first <- s_rows[1]
          s_last <- s_rows[length(s_rows)]
          border_prop <- brdr(0.4, "solid", "grey")
          ss_table <- ss_table %>%
            merge_cells(s_first:s_last, 3) %>%
            set_valign(s_first:s_last, 3, "middle") %>%
            set_left_border(s_first:s_last, 3, border_prop) %>%
            set_right_border(s_first:s_last, 3, border_prop) %>%
            set_top_border(s_first:s_last, 3:6, border_prop) %>%
            set_bottom_border(s_first:s_last, 3:6, border_prop) %>%
            set_right_border(s_first:s_last, 6, border_prop)
        }
      }
      ss_table["C7"] <- NULL
      ss_table <- ss_table %>%
        set_escape_contents(1:nrow(ss_table),1,FALSE) %>%
        set_markdown(1:nrow(ss_table), 5) %>%
        set_width(1) %>%
        set_col_width(c(1, 2, 3, 4, 5), c(0.09, 0.15, 0.1, 0.15, 0.2))
      
      para_head <- grep("Parallel Session|Invited Session", ss_table$C4)
      if (length(para_head)) {
        ss_table <- ss_table %>% set_markdown(para_head, 4:5) 
      }
      keynote_head <- grep("Keynote", ss_table$C4)
      # create anchor for keynote sessions
      if (length(keynote_head)) {
        ss_table[keynote_head, ]$C4 <- paste0('<a class="target-pos" name = "', paste0(tz, "-", ss$SESSION[1], "-keynote"), '">', ss_table[keynote_head, ]$C4, "</a>")
        ss_table <- ss_table %>% set_markdown(keynote_head, 4)
      }
      
      return(list(
        ss_session = ss$SESSION[1],
        ss_table = ss_table
      ))
    }
  )
  sessions <- sessions[session_order]
  return(sessions)
}


tz_dict <- c(
  "UTC-5" = "America/Chicago"
)
saveRDS(tz_dict, "saved_tables/timezones.rds")

# outline 
outline <- read_csv("./session_info/outline.csv", col_types = cols(
  DATE = col_date(format = "%m/%d/%Y"),
  FROM = col_time(format = ""),
  TO = col_time(format = ""),
  LOCATION = col_character(),
  TYPE = col_character(),
  TOPIC = col_character()
))
outline$FROM <- as.POSIXlt(paste(outline$DATE, outline$FROM), tz = "America/Chicago")
outline$TO <- as.POSIXlt(paste(outline$DATE, outline$TO), tz = "America/Chicago")
outlines <- lapply(tz_dict, function(tz) {
  outline_tz <- outline
  outline_tz$FROM <- with_tz(outline_tz$FROM, tz)
  outline_tz$TO <- with_tz(outline_tz$TO, tz)
  outline_tz$DATE <- as_date(outline_tz$FROM)
  outline_tz
})
ol_bytz <- lapply(outlines, generateOutlineTable)
saveRDS(ol_bytz, 'saved_tables/outline_bytimezone.rds')

# full program
full_prog_abs <- read_csv("./session_info/full_program.csv", col_types = cols(
  DATE = col_date(format = "%m/%d/%Y"),
  SESSION = col_character(),
  FROM = col_time(format = ""),
  TO = col_time(format = ""),
  SESSION_NUM = col_double(),
  LOCATION = col_character(),
  TYPE = col_character(),
  TOPIC = col_character(),
  SPEAKER = col_character(),
  TITLE = col_character()
))
# replace session number with room info
# full_prog_abs$SESSION_NUM <- full_prog_abs$LOCATION
full_prog_abs['ID'] <- 1:nrow(full_prog_abs)
full_prog <- subset(full_prog_abs, select = -ABSTRACT)
full_prog$FROM <- as.POSIXlt(paste(full_prog$DATE, full_prog$FROM), tz = "America/Chicago")
full_prog$TO <- as.POSIXlt(paste(full_prog$DATE, full_prog$TO), tz = "America/Chicago")
full_prog$TYPE_START <- as.POSIXlt(paste(full_prog$DATE, full_prog$TYPE_START), tz = "America/Chicago")
full_progs <- lapply(tz_dict, function(tz) {
  full_prog_tz <- full_prog
  full_prog_tz$FROM <- with_tz(full_prog_tz$FROM, tz)
  full_prog_tz$TO <- with_tz(full_prog_tz$TO, tz)
  full_prog_tz$DATE <- as_date(full_prog_tz$FROM)
  full_prog_tz$TYPE_START <- with_tz(full_prog_tz$TYPE_START, tz)
  full_prog_tz
})
fp_bytz <- list()
for(i in 1:length(full_progs)){
  fp <- full_progs[[i]]
  fp_bydate <- split(fp, f = as_date(fp$TYPE_START)) |> lapply(generateSessionTable)
  fp_bytz[[i]] <- fp_bydate 
}
# fp_bytz <- lapply(full_progs, generateSessionTable)
saveRDS(fp_bytz, 'saved_tables/full_program_bytimezone_date.rds')

# abstracts
prog_abs <- full_prog_abs[,c('SPEAKER', 'TITLE', 'ABSTRACT_CL', 'ID', "SPEAKER1", "SPEAKER2", "AFF1", "AFF2", "AUTHORS", "REFERENCES")]
prog_abs <- prog_abs[!is.na(prog_abs$ABSTRACT_CL), ]
abs_table <- huxtable('COL', 'ABS')
border_prop <- brdr(0.4, "solid", "grey")
for(i in 1:nrow(prog_abs)){
  ab <- prog_abs[i,]
  is_dial <- grepl("Dialogue Session", ab$TITLE)
  ab$TITLE <- paste0("<h4><a class='target-pos' name = 'abstract-", ab$ID, "'>", ab$TITLE, "</a><h4>" )
  if(!is.na(ab$SPEAKER1)){
    SPEAKER1 <- ifelse(!is.na(ab$AFF1), paste0("<p><strong>", ab$SPEAKER1, "</strong> (", ab$AFF1, ")</p>"), paste0("<p><strong>", ab$SPEAKER1, "</strong>"))
  } else {
    SPEAKER1 <- ""
  }
  if(!is.na(ab$SPEAKER2)){
    SPEAKER2 <- ifelse(!is.na(ab$AFF2), paste0("<p><strong>", ab$SPEAKER2, "</strong> (", ab$AFF2, ")</p>"), paste0("<p><strong>", ab$SPEAKER2, "</strong>"))
  } else {
    SPEAKER2 <- ""
  }
  ab$SPEAKER <- paste0(SPEAKER1, SPEAKER2)
  ab$AUTHORS <- ifelse(!is.na(ab$AUTHORS), paste0(ifelse(is_dial, "*Panelists: ", "*Authors: "), ab$AUTHORS, "*"), NA)
  ab <- ab[c('TITLE', 'SPEAKER', 'ABSTRACT_CL', "AUTHORS")]
  ab_table <-  t(as_huxtable(ab))  %>% 
    set_bottom_border(4, 1:2, border_prop) %>% set_markdown(1:4,2)
  abs_table <- rbind(abs_table, ab_table, copy_cell_props = TRUE)
}
abs_table <- abs_table[-1,-1] %>% set_width(1) %>% set_col_width(1, 1)
saveRDS(abs_table, 'saved_tables/abstracts.rds')


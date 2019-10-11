library(tidyverse)
library(tidytext)
library(jsonlite)
library(ggTimeSeries)
library(lubridate)

## Load the .json file from the FB Messenger export
messages_file <- [insert the .json file here]

## Convert the file from JSON
messages <- fromJSON(messages_file)

## Extract the Content, Sender, & Timestamp of each message
messages_content <- as.tibble(messages[["messages"]][["content"]])
messages_sender <- as.tibble(messages[["messages"]][["sender_name"]])
messages_timestamp <- as.tibble(messages[["messages"]][["timestamp_ms"]])

names(messages_content)[names(messages_content) == "value"] <- "content"
names(messages_sender)[names(messages_sender) == "value"] <- "sender"
names(messages_timestamp)[names(messages_timestamp) == "value"] <- "timestamp"

## Convert timestamp to Dates
messages_timestamp$timestamp <- as.POSIXct(messages_timestamp$timestamp/1000, origin = "1970-01-01")

## Bind each DF
message_df <- cbind(messages_timestamp,messages_sender,messages_content)

## Set a specific "document number" for each message
## Initially this was created for another project to explore topic modelling
message_df <- message_df %>%
  mutate(
    text = row_number(),
    document = paste(sender,"-",text, sep ="")
  ) 


## Count the number of messages per day to help color the Calendar visualization
messages_perday <- message_df %>%
  mutate(
    date = as.Date(timestamp)
  ) %>%
  group_by(date) %>%
  count()

## Rename COlumns
messages_perday <- plyr::rename(
  messages_perday,
  c("date" = "Date","n" = "messages")
)

## Create the initial calendar plot
p1 = ggplot_calendar_heatmap(
  messages_perday,
  'Date',
  'messages'
)

## Add in extra formatting
## The output of this plot will show the combined messages for all senders
p1 + 
  xlab(NULL) + 
  ylab(NULL) + 
  scale_fill_continuous(low = 'red', high = 'green') + 
  facet_wrap(~Year, ncol = 1)


## Calculate the number of messages per unique sender
messages_perday_perperson <- message_df %>%
  mutate(
    date = as.Date(timestamp)
  ) %>%
  group_by(date,sender) %>%
  count()

messages_perday_perperson <- plyr::rename(
  messages_perday_perperson,
  c("n" = "messages")
)

## Create the next base plot
p2 = ggplot_calendar_heatmap(
  messages_perday_perperson,
  'date',
  'messages',
  'sender'
)

## Add formatting. This next plot will show the number of messages per day per sender
p2 + 
  xlab(NULL) + 
  ylab(NULL) + 
  scale_fill_continuous(low = 'red', high = 'green') + 
  facet_grid(Year ~ sender)



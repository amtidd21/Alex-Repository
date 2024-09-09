## rough steps, carried out in a toy example below:
## 1. create a value for months (separated by | to indicate "or" for the regex) 
## 2. Say if_else(date = str_detect(X1, val), true = X1, false = NA_character_)
## 3. create a value for conferences (separated by | to indicate "or" for the regex)
## 4. Say if_else(conference = str_detect(X1, val), true = X1, false = NA_character_)
## 5. Use the fill() function from tidyr to fill "down" for the new date variable
## 6. Use the fill() function from tidyr to fill "down" for the conference variable
## 7. Filter out all of the rows with dates and conference info 

## define regex for months (should include all months in season), | means "or"
regex_date <- "January|February|March|April"

## make a toy data frame
df <- tibble(word = c("January 3", "Big Ten", "Penn State", "Ohio State", "February 5"),
             score = c("January 3", "Big Ten", "5", "3", "February 5"))

## define regex for all of the conferences
regex_conference <- "Big Ten|Big Twelve|Exhibition|Non-Conference"

## create a new column for date that is NA if nothing in the date regex is found
## create a new column for conference that is NA if nothing in the conference regex is found
df <- df |> mutate(date = if_else(str_detect(word, regex_date),
                                  true = word, false = NA_character_),
                   conference = if_else(str_detect(word, regex_conference),
                                        true = word, false = NA_character_)) 

df_filled <- df |>
  fill(date, .direction = "down") |>
  fill(conference, .direction = "down") 
df_filled |>
  filter(!str_detect(word, regex_date) &
           !str_detect(word, regex_conference)) 

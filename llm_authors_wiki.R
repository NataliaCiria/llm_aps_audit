# Load required libraries
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(httr)      # For API calls
library(jsonlite)  # For JSON parsing
library(rvest)     # For web scraping
library(stringr)   # For string manipulation

# Read previously created data
llm_top5 <- read.csv("clean_data/llm_top5.csv")
top1500_h_index <- read.csv("clean_data/top1500_h_index.csv")

get_wiki_info <- function(name) {
  # Clean the name for URL
  name_query <- gsub(" ", "%20", name)
  
  # Search for the page
  search_url <- paste0("https://en.wikipedia.org/w/api.php?action=opensearch&search=", name_query, "&limit=1&namespace=0&format=json")
  
  search_result <- tryCatch({
    fromJSON(search_url)
  }, error = function(e) {
    message("Error searching for ", name, ": ", e$message)
    return(NULL)
  })
  
  if(is.null(search_result) || length(search_result) < 4 || length(search_result[[4]]) == 0) {
    message("No Wikipedia page found for ", name)
    return(list(name = name, bio = NA, birth_date = NA, death_date = NA, 
                institution = NA, awards = NA, known_for = NA, fields = NA, 
                page_title = NA, image_url = NA))
  }
  
  # Get the page title from the URL
  page_url <- search_result[[4]][1]
  page_title <- gsub("https://en.wikipedia.org/wiki/", "", page_url)
  page_title <- gsub("_", " ", page_title)
  page_title_url <- gsub(" ", "_", page_title)
  
  # Get the page content
  wiki_url <- paste0("https://en.wikipedia.org/wiki/", page_title_url)
  
  page_html <- tryCatch({
    read_html(wiki_url)
  }, error = function(e) {
    message("Error reading HTML for ", name, ": ", e$message)
    return(NULL)
  })
  
  if(is.null(page_html)) {
    message("Could not retrieve HTML content for ", name)
    return(list(name = name, bio = NA, birth_date = NA, death_date = NA, 
                institution = NA, awards = NA, known_for = NA, fields = NA, 
                page_title = page_title, image_url = NA))
  }
  
  # Extract the first paragraph as bio
  paragraphs <- tryCatch({
    page_html %>% 
      html_nodes("p") %>% 
      html_text()
  }, error = function(e) {
    message("Error extracting paragraphs for ", name, ": ", e$message)
    return(character(0))
  })
  
  # Find the first non-empty paragraph
  bio <- NA
  if(length(paragraphs) > 0) {
    for(p in paragraphs) {
      if(nchar(gsub("\\s", "", p)) > 20) {  # At least 20 non-whitespace characters
        bio <- p
        break
      }
    }
  }
  
  # Extract infobox data
  infobox <- tryCatch({
    page_html %>% 
      html_nodes(".infobox")
  }, error = function(e) {
    message("Error extracting infobox for ", name, ": ", e$message)
    return(NULL)
  })
  
  birth_date <- NA
  death_date <- NA
  institution <- NA
  awards <- NA
  known_for <- NA
  fields <- NA
  image_url <- NA
  
  # Extract image URL from infobox
  if(!is.null(infobox) && length(infobox) > 0) {
    # Try to get the image from the infobox
    image_element <- tryCatch({
      infobox %>% 
        html_nodes("img") %>%
        html_attr("src")
    }, error = function(e) {
      message("Error extracting image URL for ", name, ": ", e$message)
      return(character(0))
    })
    
    if(length(image_element) > 0) {
      # Get the first image URL and ensure it has proper protocol
      image_url <- image_element[1]
      if(substr(image_url, 1, 2) == "//") {
        image_url <- paste0("https:", image_url)
      }
    }
    
    infobox_rows <- tryCatch({
      infobox %>% 
        html_nodes("tr")
    }, error = function(e) {
      message("Error extracting infobox rows for ", name, ": ", e$message)
      return(NULL)
    })
    
    # Function to extract data from infobox
    extract_infobox_data <- function(rows, label_pattern, multi = FALSE) {
      if(is.null(rows) || length(rows) == 0) return(NA)
      
      result <- NA
      
      for(row in rows) {
        # Extract label text safely
        label_text <- tryCatch({
          row %>% html_nodes("th") %>% html_text()
        }, error = function(e) {
          character(0)
        })
        
        # Skip if no label text found
        if(length(label_text) == 0) next
        
        # Ensure we're working with a single string for pattern matching
        label_str <- paste(label_text, collapse = " ")
        
        # Check if this row matches our pattern
        if(grepl(label_pattern, label_str, ignore.case = TRUE)) {
          # Extract value differently based on whether we want multiple items
          if(multi) {
            value <- tryCatch({
              items <- row %>% html_nodes("td li") %>% html_text()
              if(length(items) == 0) {
                # If no list items found, try to get the full text
                items <- row %>% html_nodes("td") %>% html_text()
              }
              items
            }, error = function(e) {
              character(0)
            })
          } else {
            value <- tryCatch({
              row %>% html_nodes("td") %>% html_text()
            }, error = function(e) {
              character(0)
            })
          }
          
          if(length(value) > 0) {
            result <- value
            break  # Found what we're looking for, exit the loop
          }
        }
      }
      
      return(result)
    }
    
    # Extract information with error handling
    tryCatch({
      birth_info <- extract_infobox_data(infobox_rows, "born|birth.?date")
      birth_place <- extract_infobox_data(infobox_rows, "birth.?place")
      
      if(!is.na(birth_info[1])) {
        # Parse birth info to extract date
        birth_text <- birth_info[1]
        
        # Try to extract date using regex pattern for common date formats
        date_pattern <- "\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}"
        birth_date_match <- regexpr(date_pattern, birth_text)
        
        if(birth_date_match > 0) {
          date_part <- substr(birth_text, birth_date_match, birth_date_match + attr(birth_date_match, "match.length") - 1)
          
          # Extract place by looking for text after the date
          place_start <- birth_date_match + attr(birth_date_match, "match.length")
          if(place_start < nchar(birth_text)) {
            potential_place <- substr(birth_text, place_start, nchar(birth_text))
            # Look for location after commas
            place_match <- regexpr(",\\s*[^,()]+", potential_place)
            if(place_match > 0) {
              place_part <- gsub("^,\\s*", "", substr(potential_place, place_match, nchar(potential_place)))
              birth_date <- paste(date_part, "in", place_part)
            } else {
              birth_date <- date_part
            }
          } else {
            birth_date <- date_part
          }
        } else {
          birth_date <- birth_text
        }
      }
      
      # If we have a separate birth place field and birth_date doesn't already include it
      if(!is.na(birth_place[1]) && !is.na(birth_date[1]) && !grepl(birth_place[1], birth_date[1], fixed = TRUE)) {
        birth_date <- paste(birth_date[1], "in", birth_place[1])
      }
      
      death_date <- extract_infobox_data(infobox_rows, "died|death.?date")
      institution <- extract_infobox_data(infobox_rows, "institution|alma.?mater|education|employer")
      awards <- extract_infobox_data(infobox_rows, "awards|honours|honors|prizes", multi = TRUE)
      known_for <- extract_infobox_data(infobox_rows, "known.?for", multi = TRUE)
      fields <- extract_infobox_data(infobox_rows, "field|discipline|subject|specialty|speciality", multi = TRUE)
    }, error = function(e) {
      message("Error processing infobox data for ", name, ": ", e$message)
    })
  }
  
  # Clean and trim text
  clean_text <- function(text) {
    if(all(is.na(text))) return(NA)
    
    # If it's a vector, process each element
    if(length(text) > 1) {
      cleaned <- sapply(text, function(t) {
        if(is.na(t)) return(NA)
        # Remove citations
        t <- gsub("\\[.*?\\]", "", t)
        # Remove extra whitespace
        t <- gsub("\\s+", " ", t)
        # Remove control characters and awkward formatting
        t <- gsub("\\(\\d{4}-\\d{2}-\\d{2}\\)", "", t)
        # Trim whitespace
        t <- trimws(t)
        return(t)
      })
      return(cleaned[!is.na(cleaned) & cleaned != ""])
    } else {
      # Single text processing
      if(is.na(text)) return(NA)
      # Remove citations
      text <- gsub("\\[.*?\\]", "", text)
      # Remove extra whitespace
      text <- gsub("\\s+", " ", text)
      # Remove control characters and awkward formatting
      text <- gsub("\\(\\d{4}-\\d{2}-\\d{2}\\)", "", text)
      # Trim whitespace
      text <- trimws(text)
      return(text)
    }
  }
  
  # Function to convert vector to list for multi-value fields
  to_list_if_needed <- function(field) {
    if(length(field) > 1) {
      return(as.list(field))
    } else {
      return(field)
    }
  }
  
  # Clean HTML formatting codes from bio
  if(!is.na(bio)) {
    bio <- gsub("\\.mw-parser-output.*?\\}}", "", bio)
    bio <- gsub("ⓘ", "", bio)
  }
  
  return(list(
    name = name,
    bio = clean_text(bio),
    birth_date = clean_text(birth_date),
    death_date = clean_text(death_date),
    institution = to_list_if_needed(clean_text(institution)),
    awards = to_list_if_needed(clean_text(awards)),
    known_for = to_list_if_needed(clean_text(known_for)),
    fields = to_list_if_needed(clean_text(fields)),
    page_title = page_title,
    image_url = image_url
  ))
}

# Loop through authors with error handling
author_info_list <- lapply(unique(c(llm_top5$longest_name, top1500_h_index$longest_name[1:100])), function(author_name) {
  # Clean author name from any formatting issues
  clean_name <- gsub("\\.", "", author_name)
  clean_name <- trimws(clean_name)
  
  # Print progress
  cat("Getting info for:", clean_name, "\n")
  
  # Try to get Wikipedia info with error handling
  tryCatch({
    get_wiki_info(clean_name)
  }, error = function(e) {
    message("Error processing ", clean_name, ": ", e$message)
    # Return a basic structure with NA values when an error occurs
    list(
      name = clean_name,
      bio = NA,
      birth_date = NA,
      death_date = NA,
      institution = NA,
      awards = NA,
      known_for = NA,
      fields = NA,
      page_title = NA,
      image_url = NA
    )
  })
})

# Convert the list to a data frame with error handling
author_info_df <- tryCatch({
  do.call(bind_rows, lapply(author_info_list, function(x) {
    # Skip NULL entries
    if(is.null(x)) return(NULL)
    
    # Handle list elements that might cause issues in data frame
    for (i in names(x)) {
      if (is.list(x[[i]])) {
        x[[i]] <- paste(unlist(x[[i]]), collapse = "; ")
      }
    }
    return(data.frame(as.list(x), stringsAsFactors = FALSE))
  }))
}, error = function(e) {
  message("Error creating data frame: ", e$message)
  # Create an empty data frame with the expected columns as fallback
  data.frame(
    name = character(0),
    bio = character(0),
    birth_date = character(0),
    death_date = character(0),
    institution = character(0),
    awards = character(0),
    known_for = character(0),
    fields = character(0),
    page_title = character(0),
    image_url = character(0),
    stringsAsFactors = FALSE
  )
})

# Display the first few rows of the result
head(author_info_df)

# Save the results
tryCatch({
  write_json(author_info_list, path="clean_data/author_info_list.json")
  write.csv(author_info_df, file="clean_data/author_info_df.csv", row.names = FALSE)
  cat("Data successfully saved to files\n")
}, error = function(e) {
  message("Error saving data: ", e$message)
})
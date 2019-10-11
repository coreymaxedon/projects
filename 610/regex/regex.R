library(readr)

#Dataframe setup
list_books <- list.files('books')
list_book_files <- paste("books/",
                         list_books,
                         sep = "")
row_length <- length(list_book_files)
df <- data.frame(
                  title=integer(row_length),
                  n_words=integer(row_length),
                  n_chars=integer(row_length),
                  n_individuals=integer(row_length)
                )

#Creating Function to scale up
book_info_to_df <- function(book_number,df) {
  #Find Title
  book <- read_file(list_book_files[book_number])
  title <- regmatches(book, regexpr("(T|t)itle:.+?\n",
                                    book))
  split_title <- strsplit(title, " ")
  final_title <- paste(split_title[[1]][2:(length(split_title[[1]])-1)],
                       collapse = " ")
  df[book_number, "title"] = final_title
  
  #Find Word Count
  words_in_book <- strsplit(book[1], " ")[[1]]
  num_words <- length(words_in_book)
  df[book_number, "n_words"] = num_words
  
  #Find Number of Characters
  num_char <- nchar(book)
  df[book_number, "n_chars"] = num_char
  
  #Find Number of Individuals
  all_individuals <- regmatches(book, gregexpr("(Mr\\.|Mister|Miss|Ms\\.|Ms|Doctor|Dr\\.)( [[:upper:]]([[:lower:]]*|\\.))+",     #Questioned Joe Stoica
                                               book))
  all_individuals <- unlist(all_individuals)
  n_individuals <- unique(all_individuals)
  df[book_number, "n_individuals"] = length(n_individuals)
  
  #Print out of names
  print("The individuals in this book are:")
  print(n_individuals)
  
  #Returns this books set of information
  return(df)
}

#Cycle through books
for (i in 1:21) {
  df <- book_info_to_df(i, df)
}

#Prints final DF
print(df)

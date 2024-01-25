"table"

xpath = "//table[position() = 1]"

xpath = "//table/thead | //table/tbody"

cricket <- html_table(tab1, header = TRUE)[[1]]

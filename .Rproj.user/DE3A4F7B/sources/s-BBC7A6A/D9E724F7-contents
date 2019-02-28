set.seed(1030)
# get the data into the correct dimension
desc_path <- "./raw_data/Experiment Descriptor file.csv"
desc_data <- read.csv(desc_path, stringsAsFactors = FALSE)
GDS_path <- "./raw_data/GDS531.csv"
GDS_data <- read.csv(GDS_path, stringsAsFactors = FALSE, na.strings=c("null"))

# dropping the first 2 cols ID_REF & IDENTIFIER
GDS <- GDS_data[,3:ncol(GDS_data)]
# transpose data and make it into a dataframe
GDS <- as.data.frame(t(GDS))
# test to see it transpose correctly
GDS[1,1:5] == GDS_data[1:5,3]
# test to see the length are the same
length(colnames(GDS)) == length(GDS_data[,2])

# set the col names after transposing
any(is.na(GDS_data[,1])) # make sure there are no NA column names
length(unique(GDS_data[,1])) == nrow(GDS_data) # make sure all col names unique
colnames(GDS) <- GDS_data[,1]

# check if there is any NA
check2 <- c(1,NA,2)
is.na(check2)
any(is.na(check2))

# check each col to see if there are any NA
na_result <- apply(GDS, 2, function(x) any(is.na(x)))

# create a list of col names that have NA
list_of_na <- c()
i <- 1
for (j in 1:length(na_result)) {
  if (na_result[[j]] == TRUE) {
    list_of_na[[i]] <- names(na_result[j])
    i <- i + 1
  }
}

list_of_na
#[1] "33273_f_at" "33500_i_at" "33501_r_at"

any(is.na(GDS$`33273_f_at`))
sum(is.na(GDS$`33273_f_at`))#1 NA
any(is.na(GDS$`33500_i_at`))
sum(is.na(GDS$`33500_i_at`))# 8 NA
length(GDS$`33500_i_at`) # 173 8/173 ~= 0.04624277
any(is.na(GDS$`33501_r_at`))
sum(is.na(GDS$`33501_r_at`)) # 1 NA

# So imputation is require.
# Combine the description file with the data first before imputation

# outcome aka y aka response

GDS$disease.state <- desc_data$disease.state
GDS$disease.state
state1 <- sub("      without bone lytic lesion",
              "WO", GDS$disease.state[1:36])
state2 <- sub("      with bone lytic lesion",
              "W", GDS$disease.state[37:173])
state <- c(state1, state2)
GDS$state <- state

# make sure the order is kept when renaming disease state
print(GDS[,c("state","disease.state")])

GDS$disease.state <- NULL

# find which row are NA in each column
which(is.na(GDS$`33273_f_at`))# 30
which(is.na(GDS$`33500_i_at`))# 13  20  31  65 108 125 127 160
which(is.na(GDS$`33501_r_at`)) # 5

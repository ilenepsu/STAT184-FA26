
# Homework 1 - Stat 184

# -------
# Problem 1
#   Setup Code
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# -------
# Part 1A
#   convert section to a factor with levels in the order "A", "B";
#   create a data frame named students containing all five vectors;
#   create score_matrix, with students as rows, quizzes as columns, student IDs as row names, and quiz1 and quiz2 as column names;
#   create course_record, a list containing course = "R Programming", scores = students, and a named vector cutoffs = c(pass = 70, excellent = 90).


# Converts vector section to a factor with ordered levels
section <- factor(
  c("A", "B", "A", "B", "A", "B"),
  levels = c("A", "B"),
  ordered = TRUE
  )
section

# Creates a dataframe named students with all the vectors given in setup code
students <- data.frame(
  student_id = c("S01", "S02", "S03", "S04", "S05", "S06"),
  section = c("A", "B", "A", "B", "A", "B"),
  quiz1 = c(82, 91, 76, 88, 95, 69),
  quiz2 = c(85, 89, 80, 92, 94, 74),
  passed = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
)
students

# Creates a matrix that shows the quiz scores of each student
# rbind used to give matrix # of rows matching # of students, cbind is used to bind the values of vectors quiz1 & quiz2 as columns
#   Used the below link for reference on function of cbind & rbind
#     https://www.r-bloggers.com/2024/11/how-to-combine-vectors-in-r-a-comprehensive-guide-with-examples/

score_matrix <- rbind(students)
score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- c(student_id)
score_matrix

# Creates a list called course_record with 2 variables and 1 vector
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)
course_record

# -------
# Part 1B
#   extract student S04’s second quiz score from score_matrix;
#   extract the first two matrix rows while preserving matrix dimensions;
#   extract course from course_record once with [, once with [[, and once with $;
#   briefly explain the difference between ‘[’, ‘[[’ and ‘$’

# Extraction selects from row 4, column 2
score_matrix[[4,2]]

# Extracts first 2 matrix rows
score_matrix[1:2,]

# Extracts course from list course_record using 3 different methods
course_record[1]
course_record[[1]]
course_record$course

# The differences are '[' returns a list and generally used for sublists
#   '[[' returns the element itself, thus is used if one element is wanted
#   '$' returns a named element and used if a named element is the intent

# -------
# Part 1C
#   add average, the row-wise mean of the two quizzes, to students;
#   add excellent, equal to TRUE when average >= 90;
#   select section A students whose average is at least 80;
#   return only student_id, section, and average for that subset;
#   create a named numeric vector of all student averages, using student IDs as names.

# Create new vector with quiz averages from vectorized operation (Adds two vectors then divides the sum by 2)
average <- c(quiz1+quiz2)/2
students$average <- average
students

# 
students$excellent <- students$average >= 90
students

#
subset(students, section == "A" & average >= 80, select = c(student_id, section, average))

#  
names(average) <- c(student_id)


# -------
# Problem 2
#   Setup Code
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

# -------
# Part 2A
#   inspect the result with head(), str(), dim(), and names();
#   count missing values in each column;
#   create measurements_complete using complete.cases();
#   report the sample IDs removed by the complete-case filter;
#   explain why x == NA is not a valid missing-value test.

measurements <- read.csv(text = csv_text, na.strings = c("NA", ""))
measurements

# Inspect results 
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

# Counts missing values (NA)
colSums(is.na(measurements))


measurements_complete <- measurements[complete.cases(measurements), ]

#
removed_samples <- measurements[!complete.cases(measurements), "sample_id"]
print(removed_samples)

# NA represents missing data, and using it as x == NA will cause the comparison's result to always be NA. 

# -------
# Part 2B
#   convert site and status to factors and report their levels;
#   create temp_f = temp_c * 9 / 5 + 32;
#   create ph_below_7, preserving missing pH values;
#   select complete observations from North or South whose status is "ok";
#   return only sample_id, site, temp_c, temp_f, and ph;
#   calculate the overall mean Celsius temperature and the South-site mean, ignoring missing values.

# Referenced: https://stackoverflow.com/questions/39279238/why-use-as-factor-instead-of-just-factor
# Takes subset of measurements to access only site and status and convert to factors
measurements$site <- as.factor(measurements$site)
measurements$status <- as.factor(measurements$status)

# Reports levels of site and status
levels(measurements$site)
levels(measurements$status)


# Converts data in Celsius to Fahrenheit
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

# 
measurements$ph_below_7 <- measurements$ph < 7
measurements

# Shows observations from measurements only with the status of "ok"
subset(measurements, site == "North" | site == "South" & status == "ok", select = c(sample_id, site, temp_c, temp_f, ph))

# 
overall_mean <- mean(measurements$temp_c, na.rm = TRUE)
overall_mean
south_mean   <- mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)
south_mean
                                         
# -------
# Part 2C
#   Setup Code
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

# Compute A * B and A %*% B. 
# Report the dimensions of both results and explain briefly how element-wise multiplication differs from matrix multiplication.

A
B

A * B
A %*% B

dim(A * B)
dim(A %*% B)

# Element-wise multiplication multiples values in corresponding position, ex. a[1,1] times b[1,1], white matrix multiplication takes the dot product


# -------
# Problem 3
#   Write grade_one() with given default grade boundaries
#   Use if, else if, and else to return grades A through F. Return NA_character_ when the score is missing

# -------
# Part 3A
# Function that returns letter grade depending on score
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

# Test functions with given inputs
grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

# -------
# Part 3B
# Vector of random scores to test loop
scores2 <- c(92, NA, 85, 64, 73, 89)

# Loop through scores and assign grade
grades <- rep(NA_character_, length(scores2))
for (i in seq_along(scores2)) {
  # Store one grade here
  grades[i] <- grade_one(scores2[i])
}

# Assign student IDs as names
names(grades) <- student_id
grades


# -------
# Part 3C
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  # Calculate foundational counts
  total_count <- length(x)
  missing_count <- sum(is.na(x))
  
  # Calculate statistics
  mean_val <- mean(x, na.rm = na.rm)
  sd_val   <- sd(x, na.rm = na.rm)
  min_val  <- min(x, na.rm = na.rm)
  max_val  <- max(x, na.rm = na.rm)
  
  # Combine into a named numeric vector while rounding the four statistical metrics
  results <- c(
    total_count   = total_count,
    missing_count = missing_count,
    mean          = round(mean_val, digits),
    sd            = round(sd_val, digits),
    min           = round(min_val, digits),
    max           = round(max_val, digits)
  )
  
  return(results)
}

#
plot_scores <- function(x, ...) {
  # Generate index positions 
  positions <- seq_along(x)
  
  # Plot positions vs scores
  plot(x = positions, y = x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )


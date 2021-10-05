setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if(require(pacman) == FALSE) install.packages('pacman')
pacman::p_load(tidyverse, DataExplorer, stringr, ggplot)

daytonred = "#B61E2E"

df <- read_csv("job_skills.csv")
plot_intro(df, ggtheme = theme_minimal(base_size = 18))
plot_missing(df, ggtheme = theme_minimal(base_size = 18))
glimpse(df)
table(df$Company)
df <- df %>% filter(Company=="Google") %>% select(-Company)

# Job Category
ggplot(data=df) + geom_bar(aes(x=reorder(Category, Category, function(x) length(x))),
                           fill=daytonred, position=position_dodge(width=1)) + 
  coord_flip() + theme(legend.position = "none") +
  labs(x="Job Category", y="Number of Jobs") + theme_minimal(base_size = 18)

# Job Title
titles <- sort(table(df$Title), decreasing = T)[1:20]
df1 <- data.frame(titles)
ggplot(data=df1, aes(x=reorder(Var1, Freq), y=Freq)) + 
  geom_bar(stat="identity", fill=daytonred, position=position_dodge(width=1)) + 
  coord_flip() + theme(legend.position = "none") + 
  labs(x="Top 20 Popular Job Titles", y="Number of Jobs") + theme_minimal(base_size = 18)

# Job Location
locations <- sort(table(df$Location), decreasing = T)[1:20]
df2 <- data.frame(locations)
ggplot(data=df2, aes(x=reorder(Var1, Freq), y=Freq)) + geom_bar(stat="identity", fill=daytonred, 
  position=position_dodge(width=1)) + coord_flip() + theme(legend.position = "none") +
  labs(x="Top 20 Popular Locations", y="Number of Jobs") + theme_minimal(base_size = 18)


# Job Degree Required
df$Minimum_Qualifications <- gsub("Ph.D", "PhD", df$Minimum_Qualifications)
degrees <- c("student", "BA", "BS", "Bachelor's", "Master's", "PhD")
degree_counts <- rep(NA, length(degrees))

temp_BA <- str_count(df$Minimum_Qualifications, "BA")
temp_BS <- str_count(df$Minimum_Qualifications, "BS")  
temp_enrolled <- str_count(df$Minimum_Qualifications, "enrolled")
index <- which(temp_enrolled>0)
temp1 <- temp_BA[index]
temp2 <- temp_BS[index]
temp_BA[index] <- ifelse(temp1>0, 0, temp1)  
temp_BS[index] <- ifelse(temp2>0, 0, temp2) 

for (i in 1:length(degrees)){
  degree_counts[i] <- sum(str_count(df$Minimum_Qualifications, degrees[i]), na.rm=TRUE)
}

degree_counts[1] <- degree_counts[1] + sum(temp_enrolled, na.rm = T)
degree_counts[2] <- sum(temp_BA, na.rm=T)
degree_counts[3] <- sum(temp_BS, na.rm=T)
degrees <- c("Undergraduate", degrees[-1])
df3 <- data.frame(Degree=degrees, Count=degree_counts)

ggplot(data=df3, aes(x=reorder(Degree, Count), y=Count)) + 
  geom_bar(stat="identity", fill=daytonred, position=position_dodge(width=1)) +
  coord_flip() + theme(legend.position = "none") +
  labs(x="Degree", y="Number of Jobs") + theme_minimal(base_size = 18)

# Year of Experience
Years <- c("1 year", paste(2:15, "years"))
year_counts <- rep(NA, length(Years))

for (i in 1:length(Years)){
  year_counts[i] <- sum(str_count(tolower(df$Minimum_Qualifications), Years[i]), na.rm=TRUE)
}

df4 <- data.frame(Year=1:15, Count=year_counts)

ggplot(data=df4, aes(x=reorder(Year, Count), y=Count)) + geom_bar(stat="identity", fill=daytonred,
  position=position_dodge(width=1)) + coord_flip() + theme(legend.position = "none") +
  labs(x="Year of Experiences", y="Number of Jobs") + theme_minimal(base_size = 18)

# Programming Language
languages <- c("python", "java,", "php", "javascript", "objective-c", "ruby",
               "perl", " sql","kotlin", " r,", "matlab"," c#", " c++ ", "c++,", "c++/", " c,", " c ", "c/")

language_counts <- rep(NA, length(languages))
for (i in 1:length(languages)){
  language_counts[i] <- sum(str_count(tolower(df$Minimum_Qualifications), languages[i]), na.rm=TRUE)
}

language_counts[12] <- sum(language_counts[13:15])
language_counts[15] <- sum(language_counts[16:18])
languages <- toupper(c("python", "java", "php", "javascript", "objective-c", "ruby",
                       "perl", "sql", "kotlin", "r", "matlab","c#", "c++", "c"))

df5 <- data.frame(Language=languages, Count=language_counts[c(1:13,16)])

ggplot(data=df5, aes(x=reorder(Language, Count), y=Count)) + geom_bar(stat="identity", fill=daytonred,
  position=position_dodge(width=1)) + coord_flip() + theme(legend.position = "none") +
  labs(x="Programming Language", y="Number of Jobs") + theme_minimal(base_size = 18)

subjects <- c("math", "computer science", "engineering",  "statistics", "statistical")
subject_counts <- rep(NA, length(subjects))

for (i in 1:length(subjects)){
  subject_counts[i] <- sum(str_count(tolower(df$Minimum_Qualifications), subjects[i]), na.rm=TRUE)
}

subject_counts[4] <- sum(subject_counts[4:5])
df6 <- data.frame(Subject=toupper(c("mathematics", subjects[2:4])), Count=subject_counts[1:4])

ggplot(data=df6, aes(x=reorder(Subject, Count), y=Count)) + geom_bar(stat="identity", fill=daytonred,
  position=position_dodge(width=1)) + coord_flip() + theme(legend.position = "none") +
  labs(x="Subject", y="Number of Jobs") + theme_minimal(base_size = 18)

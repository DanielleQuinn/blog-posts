# ---- Import Data ----
meta <- read.delim("afs_metadata.txt")
data <- read.delim("afs_questiondata.txt")

# ---- Load Packages ----
library(ggplot2)
library(dplyr)

# ---- Summary Stats: Sessions ----
# Number of sessions
nrow(meta)  # 26


# ---- Summary Stats: Audience ----
# Number of audience members
sum(meta$female_attendees, meta$male_attendees) # 864

# Number of female audience members
sum(meta$female_attendees)  # 343

# Proportion of female audience members (overall)
343 / 864 * 100  # 39.7 %

# Average proportion of female audience members per session
mean(meta$female_attendees / (meta$female_attendees + meta$male_attendees)) * 100  # 39.1 %


# ---- Summary Stats: Questions ----
# Number of questions
nrow(data)  # 79

# Number of questions by females
sum(meta$female_questions)  # 17

# Number of questions by males
sum(meta$male_questions)  # 62

# Proportion of questions by females (overall)
17 / 79 * 100  # 21.5 %

# Average proportion of questions by females per session
mean(meta$female_questions / (meta$female_questions + meta$male_questions)) * 100  # 19.4 %

# Sessions with no female questions
nrow(meta[meta$female_questions == 0,]) / 26 * 100  # 69.2 %
  # Number of questions asked by men in these sessions
  mean(meta$male_questions[meta$female_questions == 0])
  min(meta$male_questions[meta$female_questions == 0])
  max(meta$male_questions[meta$female_questions == 0])

# ---- Summary Stats: Question Duration ----
# Duration of questions
sum(data$seconds)  # 934 seconds

# Duration of questions by females (overall)
sum(data$seconds[data$gender == "F"])  # 208

# Duration of questions by males
sum(data$seconds[data$gender == "M"])  # 726

# Proportion of duration asked by females (overall)
208 / 934 * 100  # 22.3 %


# ---- Compare: Questions per Session ----
# Each point is one session
plot1 <- meta %>%
  select(session, F = female_questions, M = male_questions) %>%
  gather(key = "gender", value = "questions", -session)

ggplot(plot1) +
  geom_boxplot(aes(x = gender, y = questions)) +
  theme_bw(16) +
  ylab("Questions") +
  xlab("Gender")

wilcox.test(plot1$questions ~ plot1$gender)  # p < 0.01
plot1 %>% group_by(gender) %>% summarise(mean = mean(questions),
                                         sd = sd(questions),
                                         max = max(questions),
                                         min = min(questions))
# Males: 2.38 +/- 1.44 questions per session (0 - 5)
# Females: 0.65 +/- 1.16 questions per session (0 - 4)

# ---- Compare: Question Duration ----
# Each point is one question
ggplot(data) +
  geom_boxplot(aes(x = gender, y = seconds)) +
  theme_bw(16) +
  ylab("Question Duration (seconds)") +
  xlab("Gender")

wilcox.test(data$seconds ~ data$gender)  # p = 0.616
data %>% group_by(gender) %>% summarise(mean = mean(seconds),
                                         sd = sd(seconds),
                                         max = max(seconds),
                                         min = min(seconds))
# Males: 11.7 +/- 7.73 seconds per question (5 - 25)
# Females: 12.2 +/- 6.61 seconds per question (2 - 30)


# ---- First Question ----
# Does it matter who asks the first question?
data %>% group_by(session) %>%
  mutate(q1 = gender[question == 1]) %>%
  left_join(meta) %>%
  mutate(pfa = female_attendees / (female_attendees + male_attendees),
         pqa = female_questions / (female_questions + male_questions),
         questions = female_questions + male_questions) %>%
  select(session, speaker, q1, pfa, pqa, questions) %>% distinct() %>%
  ggplot() +
    geom_point(aes(x = pfa, y = pqa, col = q1, size = questions), alpha = 0.7) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed") +
    ylim(0, 1) + xlim(0, 1) +
    ylab("Proportion of Questions (Female)") + 
    xlab("Proportion of Audience (Female)") +
    scale_size_continuous(breaks = c(1, 3, 5, 7), range = c(3, 10), name = "Questions") +
    scale_colour_manual(values = c("chocolate1", "darkgreen"), name = "First Question") +
    theme_bw(16)
  
# ---- Speaker ----
# Does it matter if the speaker is male or female?
data %>% group_by(session) %>%
  left_join(meta) %>%
  mutate(pfa = female_attendees / (female_attendees + male_attendees),
         pqa = female_questions / (female_questions + male_questions),
         questions = female_questions + male_questions) %>%
  select(session, speaker, pfa, pqa, questions) %>% distinct() %>%
  ggplot() +
  geom_point(aes(x = pfa, y = pqa, col = speaker, size = questions), alpha = 0.7) +
  geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed") +
  ylim(0, 1) + xlim(0, 1) +
  ylab("Proportion of Questions (Female)") + 
  xlab("Proportion of Audience (Female)") +
  scale_size_continuous(breaks = c(1, 3, 5, 7), range = c(3, 10), name = "Questions") +
  scale_colour_manual(values = c("chocolate1", "darkgreen"), name = "Speaker") +
  theme_bw(16)

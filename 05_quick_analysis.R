library(dplyr)


#calculate average agreeement by party
people_comparisons <- read.csv("data/people_comparisons.csv", stringsAsFactors = F)

agreement_by_party <- people_comparisons %>% 
  group_by(person.latest_member.party) %>% 
  summarise(agreement = mean(agreement)) %>% 
  arrange(desc(agreement))


#calculate support by sessions
policy_divisions <- read.csv("data/policy_divisions.csv", stringsAsFactors = F)


#classify sessions
policy_divisions$division.date <- as.Date(policy_divisions$division.date)

policy_divisions$session <- case_when(
  
  between(policy_divisions$division.date, as.Date('2014-11-01'), as.Date('2015-01-31')) ~ 1,
  between(policy_divisions$division.date, as.Date('2015-02-01'), as.Date('2015-08-31')) ~ 2,
  between(policy_divisions$division.date, as.Date('2015-09-01'), as.Date('2016-01-31')) ~ 3,
  between(policy_divisions$division.date, as.Date('2016-02-01'), as.Date('2016-07-31')) ~ 4,
  between(policy_divisions$division.date, as.Date('2016-09-01'), as.Date('2017-01-31')) ~ 5,
  between(policy_divisions$division.date, as.Date('2017-02-01'), as.Date('2017-07-31')) ~ 6
  
)

#calculate support
support_by_sessions <- policy_divisions %>% 
  mutate(passed = division.aye_votes >= 225) %>% 
  group_by(session, passed) %>% 
  summarise(count = n()) %>% 
  arrange(session)
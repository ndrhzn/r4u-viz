library(httr)
library(plyr)

#get divisions------------------------------------------------------------------

get_policy_divisions <- function(id = 40, key = mykey) {
  
  divisions = data.frame()
  
  for(i in id) {
    
    message(paste('getting divisions for policy with id ', i))
    
    request = GET(url = "https://rada4you.org/", 
                  path = paste0("api/v1/policies/", i, ".json"), 
                  query = list(key = key))
    
    if(status_code(request) == 200) {
      
      response = content(request)
      
      division = ldply(response$policy_divisions, 'data.frame')
      
      if(length(division) == 0) {
        
        division = data.frame(division.id = NA, division.house = NA, division.name = NA,
                              division.date = NA, division.number = NA, division.clock_time = NA,
                              division.aye_votes = NA, division.no_votes = NA, 
                              division.possible_turnout = NA, division.rebellions = NA,
                              division.edited = NA, vote = NA, strong = NA)
        
      }
      
      division$policy_id = response$id
      
      division$policy_name = response$name
      
      divisions = rbind.data.frame(divisions, division)
      
      Sys.sleep(2)
      
    }
    
  }
  
  return(divisions)
  
}

#divisions = get_policy_divisions()

#write.csv(divisions, file = "data/policy_divisions.csv", row.names = F)


#get comparisons----------------------------------------------------------------

get_people_comparisons <- function(id = 40, key = mykey) {
  
  comparisons = data.frame()
  
  for(i in id) {
    
    request = GET(url = "https://rada4you.org/", 
                  path = paste0("api/v1/policies/", i, ".json"), 
                  query = list(key = key))
    
    if(status_code(request) == 200) {
      
      response = content(request)
      
      comparison = ldply(response$people_comparisons, 'data.frame')
      
      if(length(comparison) == 0) {
        
        comparison = data.frame(person.id = NA, 
                                person.latest_member.id = NA,
                                person.latest_member.name.first = NA,
                                person.latest_member.name.last = NA,
                                person.latest_member.electorate = NA,
                                person.latest_member.house = NA,
                                person.latest_member.party = NA,
                                agreement = NA,
                                voted = NA)
        
      }
      
      comparison$policy_id = response$id
      
      comparison$policy_name = response$name
      
      comparisons = rbind.data.frame(comparisons, comparison)
      
      Sys.sleep(2)
      
    }
    
  }
  
  return(comparisons)
  
}

#comparisons = get_people_comparisons()

#write.csv(comparisons, file = 'data/people_comparisons.csv', row.names = F)

#get personal votes for all policy divisions------------------------------------

get_personal_votes <- function(divisions_ids, key = mykey) {
  
  all_votes = data.frame()
  
  for (i in divisions_ids) {
    
    request = GET(url = paste0('https://rada4you.org/api/v1/divisions/',
                               i, '.json'),
                  query = list(key = key))
    
    if(status_code(request) == 200) {
      
      response = content(request)
      
      votes = ldply(response$votes, 'data.frame')
      
      votes$id = response$id
      
      votes$date = response$date
      
      votes$clock_time = as.character(response$clock_time)
      
      all_votes <- rbind.data.frame(all_votes, votes)
      
      Sys.sleep(2)
      
    }
    
    
  }
  
  return(all_votes)
  
}

votes <- get_personal_votes(divisions_ids = policy_divisions$division.id)

#write.csv(votes, file = 'data/personal_votes.csv', row.names = F)

#rm(divisions, comparisons, votes)

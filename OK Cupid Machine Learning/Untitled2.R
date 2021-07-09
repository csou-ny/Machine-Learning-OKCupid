#some cleaning up on the levels of education
#replace factor levels
levels(Cupid$education)[match(c("dropped out of college/university","dropped out of space camp",
                                 "working on high school", "high school","space camp","working on space camp",
                                 "dropped out of high school","graduated from high school", "graduated from two-year college"
                                 , "graduated from space camp", "dropped out of two-year college", "two-year college", "" ),
                               levels(Cupid$education))] <- "No Bachelors Degree"
levels(Cupid$education)[match(c("dropped out of law school","dropped out of masters program","dropped out of med school" 
                                 , "dropped out of ph.d program"),      
                               levels(Cupid$education))] <- "Dropped Out of Grad School"
levels(Cupid$education)[match(c("working on college/university", "working on two-year college"),
                               levels(Cupid$education))] <- "Working on Undergrad" 
levels(Cupid$education)[match(c( "working on ph.d program", "ph.d program", "graduated from ph.d program"),  
       levels(Cupid$education))] <- "Doctorate or Working on" 
levels(Cupid$education)[match(c( "working on med school", "med school","graduated from med school"), 
                       levels(Cupid$education))] <- "Med School or Working on" 
levels(Cupid$education)[match(c("graduated from law school",  "law school", "working on law school"),
                               levels(Cupid$education))] <- "Law School or Working on"
levels(Cupid$education)[match(c("graduated from masters program", "masters program", "working on masters program"),
                               levels(Cupid$education))] <- "Masters or Working on"
levels(Cupid$education)[match(c("college/university","graduated from college/university"),
                               levels(Cupid$education))] <- "Bachelors"

#religion factor levels need some work---huge amount
levels(Cupid$religion)[match(c("atheism", "atheism and laughing about it" , 
                               "atheism and somewhat serious about it", "atheism and very serious about it" ,
                               "atheism but not too serious about it"),
                               levels(Cupid$religion))] <- "atheism"
levels(Cupid$religion)[match(c("agnosticism", "agnosticism and laughing about it", 
                               "agnosticism and somewhat serious about it", "agnosticism and very serious about it",
                               "agnosticism but not too serious about it"),
                             levels(Cupid$religion))] <- "agnosticism"
levels(Cupid$religion)[match(c("buddhism and laughing about it" , "buddhism" , 
                               "buddhism and somewhat serious about it", "buddhism and very serious about it" ,
                               "buddhism but not too serious about it"),
                             levels(Cupid$religion))] <- "buddhism" 
levels(Cupid$religion)[match(c("catholicism" , "catholicism and laughing about it" , 
                               "catholicism and somewhat serious about it", "catholicism and very serious about it",
                               "catholicism but not too serious about it"),
                             levels(Cupid$religion))] <- "catholicism"
levels(Cupid$religion)[match(c("christianity" , "christianity and laughing about it" , 
                               "christianity and somewhat serious about it", "christianity and very serious about it",
                               "christianity but not too serious about it"),
                             levels(Cupid$religion))] <- "christianity"
levels(Cupid$religion)[match(c("hinduism" , "hinduism and laughing about it" , 
                               "hinduism and somewhat serious about it", "hinduism and very serious about it",
                               "hinduism and somewhat serious about it"),
                             levels(Cupid$religion))] <- "hinduism"
levels(Cupid$religion)[match(c("islam", "islam and laughing about it" , 
                               "islam and somewhat serious about it", "islam and very serious about it",
                               "islam but not too serious about it"),
                             levels(Cupid$religion))] <- "islam"
levels(Cupid$religion)[match(c("judaism", "judaism and laughing about it" , 
                               "judaism and somewhat serious about it",  "judaism and very serious about it" ,
                               "judaism but not too serious about it"),
                             levels(Cupid$religion))] <- "judaism"
levels(Cupid$religion)[match(c("other", "other and laughing about it", 
                               "other and somewhat serious about it", "other and very serious about it" ,
                               "other but not too serious about it"),
                             levels(Cupid$religion))] <- "other"
levels(Cupid$religion)[match(c(""),levels(Cupid$religion))] <- "None"


#last_online--- convert into time format and calculate number of minutes between log in

#offspring factor levels need some work
levels(Cupid$offspring)[match(c("doesn&rsquo;t have kids",  "doesn&rsquo;t have kids, and doesn&rsquo;t want any", 
                                "doesn&rsquo;t have kids, but might want them" , "doesn&rsquo;t have kids, but wants them"  ,
                                "doesn&rsquo;t want kids"),
                             levels(Cupid$offspring))] <- "No Kids"
levels(Cupid$offspring)[match(c( "has a kid", "has a kid, and might want more", "has a kid, and wants more" , 
                                 "has a kid, but doesn&rsquo;t want more" , "has kids"  ,
                                 "has kids, and might want more", "has kids, and wants more", "has kids, but doesn&rsquo;t want more"),
                              levels(Cupid$offspring))] <- "Has Kids"
levels(Cupid$offspring)[match(c("might want kids", "wants kids"), levels(Cupid$offspring))] <- "Wants Kids"
levels(Cupid$offspring)[match(c(""), levels(Cupid$offspring))] <- "No Response"

#ethnicity factor levels need some work
#turn the location into factors
#pet factor needs to be refactored

#refactor the job levels into something more manageable
levels(Cupid2$job)[match(c("artistic / musical / writer", "entertainment / media","hospitality / travel"),
                                levels(Cupid2$job))] <- "Artistic"
levels(Cupid2$job)[match(c("", "unemployed","rather not say", "other", "retired","student" ),
                        levels(Cupid2$job))] <- "Not Working or Unknown"
levels(Cupid2$job)[match(c( "military","construction / craftsmanship", "transportation" ),
                        levels(Cupid2$job))] <- "Blue Collar"
levels(Cupid2$job)[match(c("clerical / administrative",  "education / academia", "sales / marketing / biz dev"),
                        levels(Cupid2$job))] <- "Middle"
levels(Cupid2$job)[match(c("banking / financial / real estate","executive / management",
                          "law / legal services", "political / government"),
                        levels(Cupid2$job))] <- "White Collar"
levels(Cupid2$job)[match(c("computer / hardware / software", "medicine / health" ,"science / tech / engineering"),
                        levels(Cupid2$job))] <- "Technical"

Cupid$status <- ifelse(Cupid$status == "single", "Single", "Not Single")
Cupid$status <- as.factor(Cupid$status)
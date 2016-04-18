require("vioplot")
require("igraph")
require("plyr")
require("ggplot2")


setwd("~/Dropbox/school/research/nevada_donor_networks/")

state_fed_2011.df <- read.csv("FollowTheMoneyDownload20151103(101131).csv", header=T)

state_fed_2011.df <- state_fed_2011.df[-which(state_fed_2011.df$Amount<0), ]


n.donors <- length(unique(state_fed_2011.df$Contributor.id))


n.candidates <- length(unique(state_fed_2011.df$Candidate.id))

#list of donor->candidate edges
list.of.donations <- data.frame(state_fed_2011.df$Contributor.id, state_fed_2011.df$Candidate.id)

#adj.matrix for donations
donation.network <- matrix(nrow=n.candidates+n.donors, ncol=n.candidates+n.donors, 0)


is.false <- function(truth.val){

    return(!truth.val)
}


donation.matrix <- (as.matrix(data.frame(state_fed_2011.df$Contributor.id, state_fed_2011.df$Candidate.id)))


#TODO: Turn this into a function and have it return a list, including the number of donors in common for each candidate.
shared.contrib <- data.frame(matrix(nrow=n.candidates, ncol=n.candidates, 0))
names(shared.contrib) <- unique(state_fed_2011.df$Candidate.id)


for(candidate.1 in unique(donation.matrix[,2])){
    for(candidate.2 in unique(donation.matrix[,2])){
        if(is.false(candidate.1 == candidate.2)){

            contrib.to.cand.1 <- donation.matrix[which(donation.matrix[,2]==candidate.1), 1]
            contrib.to.cand.2 <- donation.matrix[which(donation.matrix[,2]==candidate.2), 1]

            if(sum(contrib.to.cand.1 %in% contrib.to.cand.2)>0){

                
                contrib.1.loc <- which(names(shared.contrib)==candidate.1)
                contrib.2.loc <- which(names(shared.contrib)==candidate.2)
            

                shared.contrib[contrib.1.loc, contrib.2.loc] <- 1
            }
            
        }

    }

}




igraph.donated <- graph.adjacency(as.matrix(shared.contrib))


party.color.name <- as.numeric(V(igraph.donated)$name)

party.color.id <- ifelse(test=party.color.name%in%as.numeric(state_fed_2011.df$Candidate.id[which(state_fed_2011.df$General_Party=="Democratic")]), yes="blue",
							no=ifelse(test=party.color.name%in%as.numeric(state_fed_2011.df$Candidate.id[which(state_fed_2011.df$General_Party=="Republican")]), yes="red", no="green"))



pdf("data_analysis.pdf")

#Summary Info:

# Data beings in January 2011 - December 2014
barplot(table(state_fed_2011.df$Date), main="Contributions by Date, Jan 2011- Dec. 2014", xlab="Date of Contributions", ylab="Number of Contributions")


# Amount Donated:

plot(density(state_fed_2011.df$Amount), "Density Plot of Amount Contributed", ylab="Density", xlab="Amount Contributed")


#candidates receiving  less than the amount which requires reporting (less than 100 dollars)
state_fed_2011.df[which((state_fed_2011.df$Amount)==-25000.0),]


# Zoomed into the high-dollar Contributors:
hist(state_fed_2011.df$Amount[(state_fed_2011.df$Amount>20000)], main="Distribution of Contributions above $20,000", ylab="Number of Contributions", xlab="Amount Contributed")

# Most frequent states:

barplot(tail(sort(table(state_fed_2011.df$State))), main="Most Frequent Origin of Contribution", xlab="State", ylab="frequency")

# Most frequent 

# Party affiliation of candidates:

pie(table(state_fed_2011.df$General_Party), main="Proportion of Number of Contributions by Party")

# Amount donated by party affiliation:

with(state_fed_2011.df, vioplot(Amount[General_Party=="Democratic"], Amount[General_Party=="Republican"], Amount[General_Party=="Nonpartisan"], Amount[General_Party=="Third-Party"], names=c("Democratic", "Republican", "Nonpartisan","Third-Party")))
title("Size of Contributions Received by Party")



# Number of candidates: 
length(unique(state_fed_2011.df$Candidate))


# Ten most frequent donors: 

ggplot(data=data.frame(Donor=as.factor(names(tail(sort(table(((state_fed_2011.df$Contributor)))),10))), N.Donations=tail(sort(table(((state_fed_2011.df$Contributor)))),10)), aes(Donor,N.Donations), fill=General_Industry) + geom_bar(stat="identity", position="dodge")+theme(axis.text.x=element_text(angle=65,vjust=1,hjust=1))+ggtitle("Ten Most Frequent Contributors")



# Most frequent industries (excluding uncoded):

ggplot(data=data.frame(Industry=as.factor(names(tail(sort(table(state_fed_2011.df$General_Industry)),10)[-10])), N.Donations=tail(sort(table(((state_fed_2011.df$General_Industry)))),10)[-10]), aes(Industry,N.Donations), fill=General_Industry) + geom_bar(stat="identity", position="dodge")+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))+ggtitle("Most Frequent Industries (Excluding Uncoded):")


plot(igraph.donated, vertex.size=2, edge.width=1, edge.arrow.size=0, vertex.label=NA, vertex.color=party.color.id, vertex.frame.color=NA, layout=layout.fruchterman.reingold(igraph.donated),main="Candidates sharing at Least One Common Contributor")


legend("topright", pch=16, col=c("blue","red","green"), legend=c("Democrat","Republican","Other"))


dev.off()


#### State wide offices only (no fed).


state_wide_contrib.df <- read.csv("state_wide_contrib_.csv", header=T)


state_wide_contrib.df <- state_wide_contrib.df[-which(state_wide_contrib.df$Amount<0), ]

state_wide_contrib.df$Date <- as.Date(state_wide_contrib.df$Date)







#####################STATE OFFICES ONLY!!!!!!!!!####################

pdf("state_wide_only.pdf")


# Mostly seems to match Gubernatorial elections.
#barplot(table(state_wide_contrib.df$Election_Year), main="Number of Contributions by Year", xlab="Year", ylab="Number of Donations")


# Confirmation
# Data beings in January 1990 - December 2014
#barplot(table(state_wide_contrib.df$Date)[-c(1:3)], main="Contributions by Date, 1990-2014", xlab="Date of Contributions", ylab="Number of Contributions")



barplot(table(state_wide_contrib.df$Election_Year), main="Contributions by Year (State Offices), 1990-2014", xlab="Date of Contributions", ylab="Number of Contributions")


# Amount Donated:

#plot(density(state_wide_contrib.df$Amount), "Density Plot of Amount Contributed", ylab="Density", xlab="Amount Contributed")


# Total Money raised in a given year by party.

(ddply(state_wide_contrib.df, .(Election_Year, General_Party), function(data){data.frame(Total_Donated=sum(data$Amount))}))



contribution_range <- ddply(state_wide_contrib.df, .(Election_Year), function(data){data.frame(Mean_Donation=mean(data$Amount), Median_Donation=median(data$Amount), Sd_Donation=sd(data$Amount))})

ggplot(state_wide_contrib.df, aes(y=Amount, x=as.factor(Election_Year))) + geom_boxplot()+ggtitle("Boxplot: Size of Contributions by Year (State Offices)")+xlab("Election Year")+ylab("Size of Contribution")


with(ddply(state_wide_contrib.df, .(Election_Year, General_Party), function(data){data.frame(Total_Donated=sum(data$Amount))}), ggplot(, aes(x=Election_Year, y=Total_Donated))+ geom_point(aes(colour=General_Party)) + geom_line(aes(colour=General_Party)))+ggtitle("Total Donated to Party Per Year (State Offices)")+xlab("Election Year")+ylab("Total Donated")


with(ddply(state_wide_contrib.df, .(Election_Year, Incumbency_Status), function(data){data.frame(Total_Donated=sum(data$Amount))}), ggplot(, aes(x=Election_Year, y=Total_Donated))+ geom_point(aes(colour=Incumbency_Status)) + geom_line(aes(colour=Incumbency_Status)))+ggtitle("Total Donated by Incumbency Status Per Year (State Offices)")+xlab("Election Year")+ylab("Total Donated")

with(ddply(state_wide_contrib.df, .(Election_Year, General_Office), function(data){data.frame(Total_Donated=sum(data$Amount))}), ggplot(, aes(x=Election_Year, y=Total_Donated)) + geom_point(aes(colour=General_Office))+ geom_line(aes(colour=General_Office)))+ggtitle("Total Donated by Office Per Year (State Offices)")+xlab("Election Year")+ylab("Total Donated")


with(ddply(state_wide_contrib.df, .(Election_Year, Election_Status), function(data){data.frame(Total_Donated=sum(data$Amount))}), ggplot(, aes(x=Election_Year, y=Total_Donated))+ geom_point(aes(colour=Election_Status)) + geom_line(aes(colour=Election_Status)))+ggtitle("Total Donated by Election Result Per Year (State Offices)")+xlab("Election Year")+ylab("Total Donated")



contrib.by.year <- ddply(state_wide_contrib.df, .(Election_Year, Contributor), function(data){data.frame(contrib.to=(data$Candidate))})



candidate.affiliation <- c()
for(candidate in contrib.by.year$contrib.to){
	candidate.affiliation <- c(candidate.affiliation, (state_wide_contrib.df$General_Party[which(unique(state_wide_contrib.df$Candidate)==candidate)]))
}

contrib.by.year$party.or.contrib <- candidate.affiliation



dem.candidates <- state_wide_contrib.df$Candidate[state_wide_contrib.df$General_Party=="Democratic"]
repub.candidates <- state_wide_contrib.df$Candidate[state_wide_contrib.df$General_Party=="Republican"]
other.candidates <- state_wide_contrib.df$Candidate[state_wide_contrib.df$General_Party=="Nonpartisan" | state_wide_contrib.df$General_Party=="Third-Party"]


 length(other.candidates)

 length(repub.candidates)

 length(dem.candidates)



pdf("state_donation_network_by_year.pdf")
#Donor Donnie network plots by year.
ddply(contrib.by.year, .(Election_Year), function(data){


subgraph <- graph.data.frame(data[,-1])

V(subgraph)$color <- ifelse(V(subgraph)$name %in% dem.candidates, "blue", ifelse(V(subgraph)$name %in% repub.candidates, "red", ifelse(V(subgraph)$name %in% other.candidates, "green", "orange")))

plot(subgraph, vertex.size=2, edge.width=1, edge.arrow.size=.05, vertex.label=NA,, vertex.frame.color=NA, main=paste(c("Contribution Network in", unique(data$Election_Year)) ))

legend("topright", pch=16, col=c("blue","red","green", "orange"), legend=c("Democrat","Republican","Other", "Contributor"))

})

dev.off()


#Candidates receiving  less than the amount which requires reporting (less than 100 dollars)
state_wide_contrib.df[which((state_wide_contrib.df$Amount)==-25000.0),]


# Zoomed into the high-dollar Contributors:
plot(density(state_wide_contrib.df$Amount[(state_wide_contrib.df$Amount>20000)]), main="Distribution of Contributions above $20,000 (State Offices)", ylab="Number of Contributions", xlab="Amount Contributed")

# Most frequent states:

barplot(tail(sort(table(state_wide_contrib.df$State))), main="Most Frequent Origin of Contribution (State Offices)", xlab="State", ylab="frequency")

# Most frequent 

# Party affiliation of candidates:

pie(table(state_wide_contrib.df$General_Party), main="Proportion of Total Number of Donations by Party (State Offices)")

# Amount donated by party affiliation:

with(state_wide_contrib.df, vioplot(Amount[General_Party=="Democratic"], Amount[General_Party=="Republican"], Amount[General_Party=="Nonpartisan"], Amount[General_Party=="Third-Party"], names=c("Democratic", "Republican", "Nonpartisan","Third-Party")))
title("Size of Contributions Received by Party (State Offices)")



# Number of candidates: 
length(unique(state_wide_contrib.df$Candidate))


# Number of Donors: 

# Ten most frequent donors: 

ggplot(data=data.frame(Donor=as.factor(names(tail(sort(table(((state_wide_contrib.df$Contributor)))),10))), N.Donations=tail(sort(table(((state_wide_contrib.df$Contributor)))),10)), aes(Donor,N.Donations), fill=General_Industry) + geom_bar(stat="identity", position="dodge")+theme(axis.text.x=element_text(angle=65,vjust=1,hjust=1))+ggtitle("Ten Most Frequent Contributors (State Offices)")


# Most frequent industries (excluding uncoded):

ggplot(data=data.frame(Industry=as.factor(names(tail(sort(table(((state_wide_contrib.df$General_Industry)))),10)[-10])), N.Donations=tail(sort(table(((state_wide_contrib.df$General_Industry)))),10)[-10]), aes(Industry,N.Donations), fill=General_Industry) + geom_bar(stat="identity", position="dodge")+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))+ggtitle("Most Frequent Industries (State Offices and Excluding Uncoded):")




dev.off()








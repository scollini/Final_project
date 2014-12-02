#Charts- I would like to show as many meaningful visualizations as possible for this project.
#This means including charts for various demographics: total population, children, 
#mothers, gender, owner, and skills on each farm. 
#I would also like to show these visuals on the 1793 map of Mount Vernon, either through 
#geo-rectifying it or through mapping the points to the image pixels. I want to show
#how the farms were unique and related to one another through the slave community. 

#This is the general idea. I have a lot of the necessary code in other homework
#but with the messy slave data. I just need to translate it here. This would
# be in a separate Rmd file. 
slave_children_1786 <- total_slaverel %>%
  filter(Skill.x == "Child") %>%
  group_by(Farm.x) 

ggplot(data = slave_children_1786, aes(x = Farm.x, stat = "identity")) + geom_bar() + theme(axis.text.x=element_text(angle = 90, hjust = 0))

slave_children_1799 <- total_slaverel %>%
  filter(Skill.y == "Child") %>%
  group_by(Farm.y) 

ggplot(data = slave_children_1799, aes(x = Farm.y, stat = "identity")) + geom_bar() + theme(axis.text.x=element_text(angle = 90, hjust = 0))




# Data Visualization and Reproducible Research

> Nicole Ely 

Learn more about me in my [GitHub profile page](https://github.com/nely7539). 


The following is a sample of products created during the _"Data Visualization and Reproducible Research"_ course.


## Project 01

In the `project_01/` folder you can find my work from the first mini project from the course. For this project, I took a look at rat sighting data in New York across 2015-2017. Rats have been known historically to carry diseases transmittable to humans. Due to this, rat control efforts in heavily populated areas is extremely important. 

### Cities with the largest number of rat sightings
- My results show the highest number of rats in New York are:
  - Brooklyn and New York City have the highest frequency of rat sightings, with over 10,000 each over the years 2015-2017
  - Bronx follows, and Staten Island and Jamaica have much lower numbers
- Since this is such a heavily populated and urbanized area, it may correlate with NYC's environment

### Number of rat sightings over years and months
- Looking at the rat sightings per each month across 2015-2017
  - Summer season (June-September) has the highest numbers of rat sightings. The frigid weather during the colder seasons of New York may cause the rats to seek and stay in shelter, however it could also present mass death. More data would be needed to further explore this idea
  - Some months had no sightings in specific years; 2015 specifically lacked sightings across months the most. 2017 had a similar pattern in October-December. This could possibly be due to rat control measures
  
### Rat sightings across address type
- Most of the rat sightings, besides those at residential addresses, occur outdoors on blocks and intersections. 
- I expected higher numbers at places like restaurants and stores (i.e. "Place Name"), as those are areas with food and other waste, but the visualization shows otherwise
  - It may be that the easiest location to verify rat sightings and utilize proper control measures is a specific address


**Sample data visualization:** 

!(figures/RatSightMonthYear.png)


## Project 02

In this project, I explored birth rates in the US across 2010-2014. Find the code and report in the `project_02/` folder. Using birth rates over location and time can allow for inference of aspects such as the economy, the existing populations in states, as well as the comparison between child-bearing age citizens vs. non-childbearing ages. 

### Interactive Visualization

- Here, I created an interactive visualization looking at birth counts by month and year
  - Across all years, there was always a pattern of decreased birth counts from 2009-2013, this may be due to the recession
  - Similarly, there was a spike in 2005-2007
  - August also had the highest birth counts, with a very sharp high point in August 2007, pre-recession

### Model Visualization

- Using a coefficient plot, I plotted estimates by year and day of month, as those were the two most significant variables in my regression
  - This plot shows that year has a far negative impact on birth numbers, and date of month teeters between positive and negative at 0, which means it has very little impact.


### Spatial Visualization

- One way to look at birth counts across a country is a spatial visualization
- Using a shapefile from the US Census, I joined the data on states in the US
  - This visualization shows birth rates across the US in 2021 
  - California and Texas have large spikes in birth rates, probably due to their already high population numbers
  - These are also possibly areas with a higher population of younger people 

**Sample data visualization:** 

!(/figures/BirthSpatial.png)


## Project 03

In this project, I explored a few different data sets

### Part 1: Tampa International Airport Weather data in 2022
- This was mainly recreating visualizations, but it was very interesting seeing the different applications of ggplot and similar packages
- One thing I noticed is that the temperatures are very different between 2022 and 2016, especially in December
- I made a figure graphing precipitation and maximum temperatures among months, which showed rain is more frequent among wamrer months
  
### Part 2: Florida Polytechnic University news articles
- For this data set, I conducted sentiment analysis using AFINN and a side-by-side barplot for all 12 months of the year
  - January, February, September, and October frequently spike at a 2.5 sentiment value, this may be due to these months being the first two months during Fall and Spring semester, where there is frequently positive news
  - I hypothesized a spike in May for graduation, but that possibly may be reflected earlier, in April, as May has a fairly low distribution

**Sample data visualization:** 

!(figures/SentimentAnalysis.png)


### Moving Forward

I feel like I have actually learned a lot from this course. I'm getting more used to the concepts of color, whitespace, and content in visualizations. I'm happy to have learned how to do interactive, spatial, and facet wrap plots especially, as these look much better than my previous plots.

I'm also glad to have refreshed on Git, as I don't frequently use it in my current position. I've actually brought up using R analysis for some of my reports I run to my department, so my colleague (who also has her bachelors in DS from Poly) and I are very excited. I feel like a lot of the tools I've learned in this class will actually be applicable to my position once we start applying R.

Another aspect that I feel like will better me in my career has been the file organization and repositories aspect of the course. I'm notoriously not the best at organizing my files (I know where everything is, but no one else does), so I plan to start organizing my folders and using r project files much more.

Thank you!!

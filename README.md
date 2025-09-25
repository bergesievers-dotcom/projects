# Lab Project 2: Sampson’s Monks

# Name: Berge Sievers

For part 0 of the project, the task was to install and load the lda, igraph, and networkd3 packages to load the required data. 

# Part 1 - Visualizing the Network

Forming the desired matrix that would necessitate running dataframes of nodes and edges, as required by the networkD3. This was done by using the SAMPLK2 Network plot.

# We plot the graph using the Igraph
A Static Visualization of the Monks by using the igraph::plot.igraph() function.
![Image Alt](https://github.com/bergesievers-dotcom/projects/blob/b8f4b7b45a37c26d2830cb5b5d52e743bc15a1df/Static_plot.png)

This chart shows that the overall structure of the monks is directed. The arrows indicate which monk is liked by another monk, and the density of the connections implies that the majority of the monks are interconnected. 
However, the ties are not evenly distributed. 
For the second plot of part 1, the idea was first to create an adjacency matrix, then convert it to nodes and edges data frames that would help to visualize the data interactively.
# Plotting an Interactive plot using networkD3
An Interactive visualization of the Monks Using the networkD3::forceNetwork() function.
![Image Alt](https://github.com/bergesievers-dotcom/projects/blob/611710e36cd0aeca58545343f725f8cbef2881d2/interactive_plot1.png)

This shows interactive Monks 

# Part 2 - Summary Statistics
# Here are the Results
$degree_out
 ROMUL_10 BONAVEN_5 AMBROSE_9   BERTH_6   PETER_4  LOUIS_11  VICTOR_8   WINF_12    JOHN_1    GREG_2   HUGH_14 
        3         4         3         3         3         3         3         3         3         3         3 
  BONI_15    MARK_7 ALBERT_16  AMAND_13   BASIL_3  ELIAS_17   SIMP_18 
        3         3         4         4         3         3         3 
$degree_in
 ROMUL_10 BONAVEN_5 AMBROSE_9   BERTH_6   PETER_4  LOUIS_11  VICTOR_8   WINF_12    JOHN_1    GREG_2   HUGH_14 
        8         8         1         5         7         2         3         2         1         1         2 
  BONI_15    MARK_7 ALBERT_16  AMAND_13   BASIL_3  ELIAS_17   SIMP_18 
        4         2         2         2         2         2         3 
$average_
[1] 1.947368
![Image Alt](https://github.com/bergesievers-dotcom/projects/blob/ced5c6b21c9d1ece994f9545bdec0cecba8036b7/barplot1.png)
From the degree analysis, we see that out-degrees are relatively balanced, with most nodes sending ties to 3 or 4 others, suggesting fairly even outward connectivity. 
However, in-degrees vary more widely, ranging from 1 to 8, indicating that some individuals, for instance, ROMUL_10, BONAVEN_5, are highly popular or central, while others receive few ties. 
The average degree of 1.9474 highlights a moderately sparse network overall, meaning most nodes are connected to only a small fraction of the group, 
reflecting selective or hierarchical relationships rather than widespread interaction. Overall, the average tie strength was 1.9474, and the most liked monk was ROMUL_10, while the least liked monk was AMBROSE_9.

# Part 3 - A Simple Model of a Social Network
For this part, a function was written to generate a random socio matrix. In this function, each monk is assigned likes at random. 
18 monks were used as n, and the probability of liking was set as default (0.3), and the maximum ti strength was also set. 

# Part 4 - Comparison: Model vs Observations
For this part of the question, the idea was to repeat the process that was carried out in part 1. However, we needed to describe the structure of the network that was simulated by the model. This was done by producing the charts shown below to help in the description. 
# Static Plot for the Randomly Generated Monk
![Image Alt](https://github.com/bergesievers-dotcom/projects/blob/4afda88e45e8919ed0c4b4375b53b7c17cec6392/Random_network.png)

# Interactive Plot for the Randomly Generated Monk
![Image Alt](https://github.com/bergesievers-dotcom/projects/blob/4afda88e45e8919ed0c4b4375b53b7c17cec6392/interactive_plot2.png)

# BarPlot for the Randomly Generated Monk
![Image Alt](https://github.com/bergesievers-dotcom/projects/blob/4afda88e45e8919ed0c4b4375b53b7c17cec6392/barplot2.png)
# The Summary Statistics are as shown below. 

$degree_out2
 monk1  monk2  monk3  monk4  monk5  monk6  monk7  monk8  monk9 monk10 monk11 monk12 monk13 monk14 monk15 monk16 
     4      4      7      4      3      6      4      5      4      4      5      4      7      5      5      5 
monk17 monk18 
     1      7 
$degree_in2
 monk1  monk2  monk3  monk4  monk5  monk6  monk7  monk8  monk9 monk10 monk11 monk12 monk13 monk14 monk15 monk16 
     5      3      5      4      5      7      6      4      4      4      5      3      4      7      4      5 
monk17 monk18 
     5      4 
$average_2
[1] 1.988095

As the results have indicated, the Average degree was greater in the randomly generated monks than it was in Sampson's monk. Moreover, when comparing the two networks, a relatively high connectivity with individuals maintaining multiple ties was observed. Additionally, the degree of distribution was also similar in the majority of the nodes that had out-degree, but relatively few for those that had in-degree. 

# Conclusion and Reflection
The results with the Monk network showed a clear structure for static and interactive plots. The results of the mean indicated that the tie strength was 1.947, and the most liked monk was ROMUL_10, 
while the least liked monk was AMBROSE_9. However, the mean tie strength for the randomly generated monk was 1.9881 and was greater than that of the monk network. The degrees for the random model network were more evenly distributed. This means that no popular or unpopular monks stood out. 
Comparing them with the Sampson's monk structure, they had no strong community structures. 
This project provided valuable insights into both technical skills and the interpretation of social network data. From a technical perspective, I learned how to generate and include plots in Markdown, manage files for GitHub submission, and calculate key network measures such as in-degree, out-degree, and average degree. The graphical visualizations were useful in identifying central nodes and structural patterns within the network
Finally, on reflection, 
It was observed that the observed networks had meaningful patterns that were not clear in the generated monks. 

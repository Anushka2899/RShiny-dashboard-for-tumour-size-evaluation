# RShiny-dashboard-for-tumour-size-evaluation

**Motivation behind the app**

In the past, creating web apps was hard for most R users because:

•	You need a deep knowledge of web technologies like HTML, CSS and JavaScript.

•	Making complex interactive apps requires careful analysis of interaction flows to make sure that when an input changes, only the related outputs are updated.

Shiny makes it significantly easier for the R programmer to create web apps by:

•	Providing a carefully curated set of user interface (UI for short) functions that generate the HTML, CSS, and JavaScript needed for common tasks. This means that you don’t need to know the details of HTML/CSS/JavaScript until you want to go beyond the basics that Shiny provides for you. As the diagram below shows, the input and output trade off plays the role of being the communication link between user and server. In addition to that, thanks to the Shiny package, one could avoid web programming and simply install the package in a machine with RStudio. 

•	Introducing a new style of programming called reactive programming which automatically tracks the dependencies of pieces of code. This means that whenever an input changes, Shiny can automatically figure out how to do the smallest amount of work to update all the related outputs.

•	R is a language and environment for statistical computing and graphics which is easy to use and provides different packages to create the visually appealing and interactive applications without much hassle.  

**What are Waterfall Plots?**

Waterfall plots are graphic illustrations of data that can vary from audio frequencies to clinical trial patient information and results. Waterfall plots have begun to gain popularity in the field of oncology clinical trials. It may be used to present each individual patient’s response to a particular drug based on a parameter, such as tumor burden.  They can serve as a novel efficacy measure, in terms of presenting the reduction in tumor burden for each subject .

The horizontal (x) axis across the plot may serve as a baseline measure; vertical bars are drawn for each patient, either above or below the baseline. The vertical (y) axis may be used to measure maximum percent change from baseline, e.g., percent growth or reduction of the tumor. Those vertical bars that are above the line represent non-responders or progressive disease. Vertical bars below the baseline (x) axis are drawn for each patient that has achieved some degree of tumor reduction, often depicted as negative percent.

Unfortunately, this visual display suffers a number of limitations including (1) potential misguidance by masking the time dynamics of tumor size, (2) ambiguous labelling of the y-axis, and to overcome these drawback, other plots such as Spider plots can be used.

**What are Spider Plots**

Spider plots in oncology are used to depict changes in tumor measurements over time, relative to the baseline measurement. The resulting graph looks like the legs of a spider and hence the name. To illustrate the incorporation of example treatment information into the plot, the subjects in this dataset were randomly placed into control and drug treatment arms. Also, the follow-up time was restricted to 240 days (8 months) and here is the resulting spider plot
Unlike a waterfall plot, where the best overall response from baseline for every subject is displayed, in a spider plot % change from baseline for a subject is displayed at every significant time-point. In spider plot, every subject is represented as a line that starts from the baseline. Depending on whether there is an increase or decrease in tumor size, the lines then are plotted according to the change. This way the % change from baseline for each subject is plotted across the entire study duration. Spider plots are easier to interpret when there are not too many subjects. Otherwise the plot would look more cluttered and harder to read.  

**Creation of Web App**

A web application was created to explore and visualize the data in more efficient way. For the creation of the app, Shiny is used. It is an R package that allows you to easily create rich, interactive web apps. Shiny allows you to take your work in R and expose it via a web browser so that anyone can use it. 
There are two key components of Shiny- the UI (short for user interface) which defines how your app looks, and the server function which defines how your app works. 

A Shiny app is a web page (UI) connected to a computer running a live R session (Server) Users can manipulate the UI, which will cause the server to update the UI’s displays (by running R code)

•	ui (User Interface) - nested R functions that assemble an HTML user interface for the App. It is in charge of the contents and style of the App. FluidPage- Shiny uses the function fluidPage to create a display that automatically adjusts to the dimensions of your user's browser window. You lay out the user interface of your app by placing elements in the fluidPage function.

•	server - a function with instructions on how to build and rebuild the R objects displayed in the UI. Its arguments are the inputs and the outputs of the App. In this part is where our R code is going to be included. The user can interact with the server in such a way he/she can lead the process behind the App. The user declares input or variables that the developer design for being modified by him/her. Afterwards, the server uses this input for conditionally returning the output.

•	shinyApp - combines ui and server into an App.

<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>
Data Cleaning - How to remove outliers & duplicates
==============================
  
After learning to [read formhub datasets into R](http://modilabs.github.com/formhub.R/demo/Basics_of_formhub.R.html), you may want to take a few steps in cleaning your data. In this example, we'll learn step-by-step how to select the variables, paramaters and desired values for outlier elimination. 

Begin with reading in your data set... 
  
  ```{r read, warning=FALSE, message=FALSE}
library(formhub)
formhubData <- formhubRead("~/Downloads/My_Data_Set.csv", "~/Downloads/My_Form.json")
my_data <- as.data.frame(formhubData)
```

followed by selecting a variable that you want to do outlier work on. Let's look at the total amount of female pupils per school for this particular data set, labeled as `num_students_total_gender.num_students_female`.  

  ```{r plot_completiontime, warning=FALSE, message=FALSE, fig.height=5, fig.width=9}
library(ggplot2)
qplot(data = my_data, x = num_students_total_gender.num_students_female) + ylab("Number of Schools") 
  ```

A quick eye-balling of the plot tells us that there are a couple of female student outliers that are quite high - as indicated by the extension of x-axis to 5000. Zooming in our plot may help clarify:   

  ```{r plot_completiontime, warning=FALSE, message=FALSE, fig.height=5, fig.width=9}
qplot(data = my_data, x = num_students_total_gender.num_students_female) + ylab("Number of Schools") + scale_x_continuous(limits=c(0,2000))
  ```

We see our initial cursory reading to be true, as the vast majority of schools have less than 500 female pupils. For the sake of crudely setting our outlier paramaters, let's say that any facility reporting to have over 1000 female pupils will be counted as an outlier. Using the `formhubOutlier` function, we will turn any such values into NA:

  ```{r read, warning=FALSE, message=FALSE}
library(data.table)
"forumhubOutlier" = function(dt, cols, rows, value) {
  if (any(rows)) {
    set(dt, rows, cols, value) }
  }

formhubOutlier(my_data, 'num_students_total_gender.num_students_female',
                 which(my_data$num_students_total_gender.num_students_female > 1000), NA)
  ```
  ```{r plot_completiontime, warning=FALSE, message=FALSE, fig.height=5, fig.width=9}
qplot(data = my_data, x = num_students_total_gender.num_students_female) + ylab("Number of Schools")
  ```

It is also possible to change the value of more than one data point. Using the same outlier limit of 1000 for instance, we can change both the number of female pupils and the total number of pupils to NA like so:

  ```{r read, warning=FALSE, message=FALSE}
formhubOutlier(my_data, c('num_students_total_gender.num_students_female', 'num_students_total_gender.num_students_total'),
               which(my_data$num_students_total_gender.num_students_female > 1000, NA )
  ```

Finally, instead of of changing outliers to NA, we can make them equal to 1000:

formhubOutlier(my_data, 'num_students_total_gender.num_students_female',
              which(my_data$num_students_total_gender.num_students_female > 1000), 1000)

  
Saving your dataset
-----------------------
Don't forget to save your new data set!
```{r  warning=FALSE, message=FALSE, fig.height=5, fig.width=7}
  write.csv(my_data, "~/Desktop/my_data_OutlierCleaned")
```  
  
  
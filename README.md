# Crowding-Time-VR
This repository contains the data and analysis code for a project on the relationship between crowding and time perception, tested in a virtual reality setting.

# Video demos
Here is a demo of the train environment at the lowest crowding level (https://www.youtube.com/watch?v=dXZu23cMfEw): 

[![level one](https://img.youtube.com/vi/dXZu23cMfEw/0.jpg)](https://www.youtube.com/watch?v=dXZu23cMfEw)


And here is the environment at the highest crowding level (https://www.youtube.com/watch?v=0E4O-S3givg): 

[![level five](https://img.youtube.com/vi/0E4O-S3givg/0.jpg)](https://www.youtube.com/watch?v=0E4O-S3givg)


The below video shows the setting of the VR experiment in the lab. The wristband on the right hand is for recording physiological data. The tablet screen shows the real-time cardiac recording from the wristband (connected to the tablet via bluetooth).

[![level five](https://img.youtube.com/vi/Rc7tcL7Pz60/0.jpg)](https://www.youtube.com/watch?v=Rc7tcL7Pz60)



## Data
Data contains the participants responses to questions and surveys, cardiac data collected using the E4 wristband, and time logs of start and end of each trip (to align with cardiac data).

### E4
Cardiac data recorded by E4 wristband

### time logs
Start and end timestamp of trips to be aligned with cardiac data

### qualtrics
Participants responses recorded using Qualtrics online platform

## Analysis

### analyze -> setup
Reading the data and preparation for analysis. Trip data are stored in a dataframe called *df_all*, and subject data are stored in a dataframe named *df_subj*

### analyze -> main
Statistical tests



# GFC
Global Fund for Children
Project Statement

 “Global Fund for Children wants to use data on the organizational development of grantee partners and inputs provided by GFC during the course of the funding relationship to better understand the link between the services and funding provided and self-reported changes in organizational health. This will assist GFC to better understand which interventions are most successful at contributing to positive organizational changes. As a result, GFC can more effectively target services to enable organizations that aim to improve the lives of children and youth affected by poverty, injustice and discrimination worldwide to be more effective at their work.”
 
 
Skills 
•	Context/experience or an interest in international children’s rights work
•	Novice/Intermediate predictive modeling and machine learning *
•	R or Python for model building and automating data cleaning/joining tasks *
•	Qualitative text analysis/NLP *
•	Data Analysis/Visualization with any tool*
Main Task
The main task is to see if there are links between any of the various inputs (or combination of inputs) GFC is providing and positive changes in the outputs/outcomes they are interested in tracking. This is likely a ‘machine learning light’ problem, as it involves determining which of a host of input and characteristic variables are most important to the target variables. We say ‘light’ because it is not a problem where a predictive model with high accuracy is expected to be made that would determine grantmaking decisions. Rather, if a model can show that some inputs have a higher probability of being more effective in leading to some outputs, it would be a useful piece of information to consider in addition to many other factors that go into in to the grantmaking process. The outputs, inputs, and organization characteristics that are located in the data provided are listed on the second page.
Parts/Steps of Main Task (stars correspond to relevant skills)
•	Join all the data together at the organization level**
o	Document schema and fields use to make joins
•	Create some new organization-level variables, such as: 
o	Total award ‘points’ (from weighted awards data), total activity ‘points’ (from weighted activity data), normalized or just percentage based outcome variable, change over time variables, etc. **
•	Automating the above from the spreadsheets provided so that when new data is available this doesn’t all have to be done manually again. *
•	Oh yeah the analysis part! With a clean dataset and knowledge of the project goals, its up to you on what could be most useful. ****
•	Some analysis of the narrative text fields where grantees describe impact and/or give program exit interviews. There’s meaningful information here, but can any of it be systematically pulled out? *
•	Descriptive analysis and/or visualization of the data. What’s interesting about it to you? What insights jump out? Especially once new variables such as total weighted activity per org are added, there could be some new ways of looking at the data as a whole. *
The Data
All of the data is in the GitHub repo. The different categories and variables within them are below:


Outputs:

·         Budget increase-can be found in all grant data spreadsheet

·         Number directly served-can be found in all grant data spreadsheet

·         Visibility-In “awards data” tab of “major awards” spreadsheet. We do not have data for social media followers but would be interested in seeing if we can find that information.

·         OCI increase-can be found in all grant data spreadsheet and OCI generator.

·         Program outcomes-can be found in all grant data spreadsheet. The program outcomes are more accurate since we incorporated an outcome question in our online application in 2012, but as I have noted, the grantee partners can choose from dozens of program outcomes to report from, so the challenge will be aggregating those different categories.
 
Inputs:

·         Total grant amount per partner-can be found in total grant amount per partner spreadsheet. Please be aware that some supplemental grants have been awarded to organizations that never received primary grants. We do not expect to track the outputs of those organizations. All Affinity Grants should be disregarded. Those grants are used for membership fees in other networks.

·         Type of grants, particularly organizational development awards- can be found in all grant data spreadsheet

·         Grant use-can be found in all grant data spreadsheet, Column V, “Grant Use Sentence.” These are narrative sentences-is there a way to analyze if there is a connection about what the grants were used for and outputs?

·         Leveraging-can be found in all activities spreadsheet

·         Site Visits-can be found in all activities spreadsheet

·         Knowledge Exchanges-can be found in all activities spreadsheet

·         Other inputs-can include GFC-led convenings, additional touch, emails, phone calls, and legal referrals. These activities are not very numerous.
 
Characteristics:

·         Region

·         Country

·         Baseline budget size- can be found in all grants data spreadsheet

·         Baseline OCI-can be found in all grant data and OCI generator


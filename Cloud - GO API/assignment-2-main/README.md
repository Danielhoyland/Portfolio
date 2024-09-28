# Assignment 2 Group Gamers
## Info
* This is our group repo. All the files is in the repo so you can clone this repo and get all of our files.
* We tried to host our dataset with realtime database but found out it wasnt easy to use it for this assignment so instead we just used the csv file. 
* We downloaded the csv file from the website not directly from the first link. So our data will not give the same precentage as it actually should since we use the best reneweble data from our csv which is not the same as the csv data in the first link. Everything else is working very good.
* Our webhooks do not save after program stops as we didn't get enough time to implement that

## How to use the program when using our skyhigh hosting:
* Use this link to try our hosting and follow the the deafualt hnadler page
 - http://10.212.174.20:81/
* You can then use one of our endpoints by using one of the lines in the deafualt handler

## How to use the program when hosting yourself:
* Clone the repository
* open the file in an IDE like goland
* right click the main.go file in goland and press the run button
* open http://localhost:8080/ in a browser then you will come to the deafualt handler and can see your options

## Looking at first endpoint
* This endpoint takes the Iso code of a country and then takes a bool for if neighbours is going to get shown or not.
* When this is chosen in the link  like this: 
* Then the page will show Country, newsest year, isocode and the percentage of renewables.
* Some isocode that we recommend to test, NOR, USA, RUS, SWE.
* example links:
  - http://localhost:8080/energy/v1/renewables/current/nor?neighbours=true
  - http://localhost:8080/energy/v1/renewables/current/usa?neighbours=false
  - http://localhost:8080/energy/v1/renewables/current/fin

## Looking at second endpoint
* This endpoint takes a countries iso code and then a starting year and a end year.
* When this is chosen like in link like this:
* Then the page will show Country, isocode, year and percentage of renewables for every year from chosen start year to chosen end year including the starting and ending year.
* Example links:
  - http://localhost:8080/energy/v1/renewables/history/nor?begin=1960&end=1970

## Looking at third endpoint
### POST requests
* POST requests on this endpoint take data in the body of the request and using that data creates a webhook.
* Example data:
```
{
   "url": "https://url.to.recieve.response.com",
   "country": "NOR"
}
```
* Example link: http://localhost:8080/energy/v1/notifications/
### POST requests in POSTMAN:
  - Enter the example link in the URL bar
  - Select POST request from the drop down menu on the left
  - Under the URL bar select "Body", under that select "raw" and that should make another drop down menu appear to the right that initially is selected as "text", click it and select "JSON"
  - Into the large text field below copy paste the example data above. You can change the url and country values if you wish. 
  - Click send and if everything is correct that should create a webhook and you will recieve a response (bottom of screen) with a unique ID for the webhook you just created!!!
* Data explanation
  -  url: The url to send a response to if webhook gets triggered
  - country: The country that will trigger the webhook (if a get request is sent to the first or second endpoint that includes the country given)
* This endpoint also returns an auto generated ID for your registered webhook, you can use that ID later to delete and see that specific webhook
### GET requests
* GET requests on this endpoint returns information on webhooks. You can either search without tags to return all registered webhooks or with an ID to return a specific one
* Example links:
  - http://localhost:8080/energy/v1/notifications/ - get all
  - http://localhost:8080/energy/v1/notifications/(webhook_ID) - get specific
### DELETE requests
* DELETE requests take an ID as a parameter in the link, and it deletes that webhook
* Example link:
  - http://localhost:8080/energy/v1/notifications/(webhook_ID))

## Looking at status
* This shows how it is going with countries API, our firebase database, amount of webhooks and the version and uptime of our page/program.

## Testing
* Our tests is in the dir test.
* To test our tests you can either in and IDE like goland right click on each test and press run to test one test at the time or right click the package test and hover run and press go test assignment 2 something and then you can test all our test at the same time.
* Another way to test our tests is by opening terminal and go to the test dir in either an IDE or like cmd and use the command go test inside that dir.
* If it comes back as PASS it means that test or if you use terminal It means all passed.


## Deployment steps
* We used NTNU's skyhi to host a viritual machine, which was used as our server
* To simplify the deployment process we used docker to create an image (docker code in the dockerfile in the workin directory)
* The dockerfile copies all relevant files into its image and runs it on port 81
* The API is then usable with the link "10.212.174.20:81", it *should* be online 24/7, but skyHi might have some downtime. 
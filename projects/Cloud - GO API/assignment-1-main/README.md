# assignment-1



## Getting started

First thing to do is to check out https://daniehoy.onrender.com
Here you will see 3 links that you can click on at will get you to diffrent endpoints.

## First endpoint
The first link is for uniinfo. Here it will show all universities if you dont try to add a partial or full name after. It should be /unisearcher/v1/uniinfo/{partial or full uni name}. Addionally it can be used with and / at the end after the partial or full university name. If what you typed in doesn't correspond to any universities then "404 page not found" will be shown.

## Second endpoint
The second link is for neighbour unis. WHen you first press the link in it will show "404 page not found". Don't be afraid, this is because you don't have anything filled in after. The endpoint takes a full name country lower or upper case after of which it takes partial or full name of university and then the limit of how many to show. The page itself gives information about unis in neighbouring countries with the uni name that you give som don't expect anything for searching norway with norwegian. The link show have something like this after the main link: /unisearcher/v1/neighbourunis/Norway/uni?limit=1000. You will still get an 404 page if the it can't find any unis with the name or country that you had given.

## Third endpoint
This endpoint is only for diagnosis. Pressing the third link takes you right there, no other adding to the link. This shows an code of 200 if countries api is ok and if the universities api is ok. After of which it shows the uptime of the site and the version of this site. There is not much mor to this site.

## The program
If you want to try this program you should probably use an IDE like Goland from jetbrains. From there open the file. In the first directory you see one more directory and a main with a go.mod. The go.mod show to the name of the directory this project in and is enforcing the go versjon to the project. In the main file its refering to the functions in the other files further into the files. If this linking wasn't there the program wouldn't be running. This is also the part where we can edit how we want our link is going to be.

In the other directory, the v1 we see 3 new dir to the "main" programs that is the diffrent sites. In the constant go file we add our constants like filepath, url path and so on to be of easy use in the other files and keeping it more readeble and changeble. The default go file its the basic page and first page when you press https://daniehoy.onrender.com and gives you the links to the diffent endpoint. The get api get the api and unmarshel them when called upon. This makes it a easy use in other program again. The struct.go file is for the structure of the calling of information from the api's. 

In the diag dir its a singualar file where the most advanced bit is the time handeling of how long the site has been up. Its in its own function there. Other than that the program call upon the api's to check if they are good and writes this information to the diag site.

The neighboursHandler is the most difficult program to understand. It takes both the api's and tries to match the country name to the country name the user added. It compares it to the diffrent countries from the country API and get its borders information, meaning what countries that is neighbours to that country. That list is of 3 letters for each country in that list so with that information we compare that again to the and comapare it to the cca3 which is a 3 letter code for each country and keep each of those 3 countries information. After that we take the university partial or not and find the universities that have a matching name and compare those universities cca2 code which is the 2 letter country code they are in with the country cca2 country code of the countries that are neighbour to the one we entered. After that we keep the information about whcih unis that matches all of our comparisons and with the help of the structs get the output we want.

THe last dir is Univerisity info. This compared to the last one is very easy to understand. We get take in the API's and start comparing the partial or full name of the uni to the unis in the unis API and by comparing the input with the names of the universities. After that we save that info and with the help of the structs get to write the information we wanted to the page. 

## How to try this program on your own PC.
By using the Goland from jetbrains it will be easy. First thing is to go to main.go. When you are the you try to see if you can press like the green "play button" up beside where it should say "go build main.go". If this isn't an option then press right click in the editior where its the main file is getting edited. Then press run "go build main.go". When this works it will take some time typically 10-30 seconds. When its done you should be able to use http://localhost:8080/ and you should be placed on the same page as if you pressed my render link. You can do the same stuff with this as if you were on my render page. If you want to add stuff or edit it and check how it looks now then you just have to save the changes and restart the the build.

Hope this helps!

package com.example.inkreader

import android.os.Bundle
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Box
import androidx.compose.ui.res.painterResource
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.wrapContentHeight
import androidx.compose.foundation.layout.wrapContentWidth
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.scale
import androidx.compose.ui.graphics.ColorFilter
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.em
import androidx.compose.ui.unit.sp
import androidx.navigation.NavHostController
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import com.example.inkreader.ui.theme.InkReaderTheme


class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        loadDataFromStorage(this)
        setContent {
            InkReaderTheme (
                darkTheme = DataStorage.isDarkModeEnabled.value
            )  {
                // A surface container using the 'background' color from the theme
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colorScheme.background
                ) {
                    NavFunc()
                }
            }
        }
    }
}

//navigation function, is the top level function called in mainActivity
@Composable
fun NavFunc(
    navController: NavHostController = rememberNavController(),
    startDestination: String = "home"
) {
    NavHost(
        navController = navController,
        startDestination = startDestination
    ) {
        composable("home") {    //Home route
            HomeScreen(
                onNavigateToBook = { navController.navigate("book") },
                onNavigateToOverview = { navController.navigate("overview") },
                onNavigateToCalender = { navController.navigate("calender") },
                onNavigateToSettings = { navController.navigate("settings") }
            )
        }
        composable("book") {//Adding book screen
            AddBookScreen (
                onNavigateToHome = { navController.navigate("home") }
            )
        }
        composable("overview") {//overview screen
            BookOverviewScreen (
                onNavigateToHome = { navController.navigate("home") }
            )
        }
        composable("calender") {//calendar screen
            CalenderScreen (
                onNavigateToHome = { navController.navigate("home") }
            )
        }
        composable("settings") {//Settings screen
            SettingsScreen (
                onNavigateToHome = { navController.navigate("home") }
            )
        }

    }
}

//Home screen composable, needs the lambda for every page it navigates to.
@Composable
fun HomeScreen(
    onNavigateToBook: () -> Unit,
    onNavigateToOverview: () -> Unit,
    onNavigateToCalender: () -> Unit,
    onNavigateToSettings: () -> Unit
) {
    Column(
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Bottom,
        modifier = Modifier
            .fillMaxSize(1.0f)
    ){
        var streak = 0 //streak counter
        var tempDay = DataStorage.day
        var tempMonth = DataStorage.month
        var tempYear = DataStorage.year
        //counts from 0 to amount of different data in the list
        for (i in 0 until DataStorage.readingDates.size) {
            //if the day minus i isn't before the first day of the month
            if (DataStorage.day - i > 0 && i > 0) {
                tempDay -= 1 //go one extra day back in time if its not the first repeat
            } else if (i > 0) {
                //day is zero then go back a month and get the required data
                //also this only happens if its not the first month of the year
                if (tempMonth - 1 != 0) {
                    tempMonth -= 1
                    tempDay = daysInMonth(tempYear, tempMonth)
                } else {
                    //if its the first month of the year then go to month 12 one year earlier
                    tempYear -= 1
                    tempMonth = 12
                    tempDay = daysInMonth(tempYear, tempMonth)
                }
            }
            //puts these temp dates into DataInfo type object to compare
            val tempDateObject = DateInfo(tempDay, tempMonth, tempYear)
            //if it finds the date then add 1 to the streak
            val dateStuff =
                DataStorage.readingDates.find { it.theDate == tempDateObject && it.read >= DataStorage.pageGoal.intValue }
            if (dateStuff != null) {
                streak += 1
            } else if (tempDateObject != DataStorage.currentDateObject) {
                //if not then go out of the for loop
                break
            }
        }
        // ------ TOPBAR -------
        Row(
            horizontalArrangement = Arrangement.Center,
            verticalAlignment = Alignment.CenterVertically,
            modifier = Modifier
                .weight(0.7f)
                .fillMaxWidth()
                .fillMaxHeight()
                .background(color = MaterialTheme.colorScheme.primaryContainer)
        ) {
            Text(
                text = "InkReader",
                letterSpacing = 0.1.em,
                color = MaterialTheme.colorScheme.onPrimaryContainer,
                fontSize = 30.sp,
                fontWeight = FontWeight.Bold,
                textAlign = TextAlign.Center,
                modifier = Modifier
                    .padding(16.dp)
            )
        }
        // ------ STREAK -------
        Box(
            contentAlignment = Alignment.Center,
            modifier = Modifier
                .weight(2.0f)
                .fillMaxSize()
        ) {

            val color = MaterialTheme.colorScheme.primary
            Image(
                colorFilter = ColorFilter.tint(color),
                painter = painterResource(id = R.drawable.inksplatter),
                contentDescription = null,
                modifier = Modifier
                    .size(330.dp, 250.dp)
                    .scale(1.3f)

            )
            StreakInkSplatter(streak, Modifier.align(Alignment.Center))
        }
        // ------ AQUARIUM --------
        Row(
            horizontalArrangement = Arrangement.Center,
            modifier = Modifier
                .weight(2.5f)
                .padding(top = 40.dp)
                .fillMaxWidth(1.0f)
                .fillMaxHeight(0.4f)
                .wrapContentWidth(Alignment.CenterHorizontally)
                .wrapContentHeight(Alignment.Top)
                .background(MaterialTheme.colorScheme.primaryContainer)
                .border(
                    width = 2.dp,
                    color = MaterialTheme.colorScheme.primary,
                    shape = RoundedCornerShape(5.dp)
                )
                .padding(10.dp)
        ) {
            //Fish tank "animation" based of streak
            FishAnimation(streak)
        }
        //Spacer(modifier = Modifier.weight(1.75f))
        // ------ NAVBAR
        Column(
            modifier = Modifier
                .height(105.dp)
                .padding(bottom = 4.dp)
        ) {
            Row(
                horizontalArrangement = Arrangement.SpaceAround,
                modifier = Modifier
                    .fillMaxWidth()

            ) {
                //navigation to add book section, 0.5f to make it fill half the screen
                NavButtons(
                    homeCallback = onNavigateToBook,
                    buttonText = "Add a book"
                )
                //navigation to book overview section, no value on fill to fill the rest in the row
                NavButtons(
                    homeCallback = onNavigateToOverview,
                    buttonText = "Book overview"
                )
            }
            Spacer(modifier = Modifier.height(5.dp))
            Row(
                horizontalArrangement = Arrangement.SpaceAround,
                modifier = Modifier
                    .fillMaxWidth()
            ) {
                //navigation to calendar section, 0.5f to make it fill half the screen
                NavButtons(
                    homeCallback = onNavigateToCalender,
                    buttonText = "Calendar"
                )
                //navigation to audio section, no value on fill to fill the rest in the row
                NavButtons(homeCallback = onNavigateToSettings,
                    buttonText = "Settings"
                )
            }

        }
    }
}


//the composable function for setting the correct fish tank image.
@Composable
fun FishAnimation(streak: Int) {
    //fish images list
    val fishImages = listOf(
        R.drawable.fish1,
        R.drawable.fish2,
        R.drawable.fish3,
        R.drawable.fish4,
        R.drawable.fish5,
        R.drawable.fish6,
        R.drawable.fish7,
        R.drawable.fish8,
        R.drawable.fish9,
        R.drawable.fish10,
        R.drawable.fish11,
        R.drawable.fish12,

        )
    //if streak is over the the fish images size minus 1(indexing) then set it to the highest index
    val currentFishIndex = if(streak>fishImages.size-1){
        fishImages.size-1
    }
    else{
        //else set it to streak amount
        streak
    }
    //paint image that got chosen
    Box(
        modifier = Modifier
    ) {
        Image(
            painter = painterResource(fishImages[currentFishIndex]),
            contentDescription = null,
        )
    }
}

//function to draw the streak counter
@Composable
fun StreakInkSplatter(streak: Int, modifier: Modifier = Modifier) {
    Column(
        verticalArrangement = Arrangement.Center,
        modifier = Modifier
            .fillMaxWidth(0.35f)
            .fillMaxHeight()
            .aspectRatio(1f)
            .border(2.dp, color = MaterialTheme.colorScheme.primaryContainer, CircleShape)
            .clip(CircleShape)
            .background(Color.Transparent)
            .padding(top = 4.dp)
    ) {
        Text(
            text = "Streak:",
            textAlign = TextAlign.Center,
            fontSize = 20.sp,
            color = MaterialTheme.colorScheme.primaryContainer,
            modifier = Modifier
                .fillMaxWidth()
                .wrapContentWidth()
        )
        Text(
            text = "$streak",
            textAlign = TextAlign.Center,
            fontSize = 40.sp,
            color = MaterialTheme.colorScheme.primaryContainer,
            modifier = Modifier
                .fillMaxWidth()
                .wrapContentWidth()
        )
    }
}

//function to draw the button that navigates to a certain page
//created to reduce code lenght
@Composable
fun NavButtons(
    homeCallback: () -> Unit,   //The callback function that calls Navhost.navigate("route")
    buttonText: String          //The text within the button
) {
    //the button itself
    Button(
        modifier = Modifier
            .width(180.dp)
            .height(50.dp)
            .border(
                width = 1.dp,
                color = MaterialTheme.colorScheme.primary,
                shape = RoundedCornerShape(5.dp)
            ),
        colors = ButtonDefaults.buttonColors(
            containerColor = MaterialTheme.colorScheme.onPrimary,
            contentColor = MaterialTheme.colorScheme.primary),
        shape = RoundedCornerShape(5.dp),
        onClick = homeCallback,
    ) {
        Text(text = buttonText)
    }
}
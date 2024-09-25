package com.example.inkreader

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
//composable for the calendar screen
@Composable
fun CalenderScreen(
    onNavigateToHome: () -> Unit
) {
    //selected month and year start on current date
    var selectedMonth by remember { mutableIntStateOf(DataStorage.month) }
    var selectedYear by remember { mutableIntStateOf(DataStorage.year) }
    //screen width
    val screenWidth = LocalConfiguration.current.screenWidthDp.dp
    //val screenHeight = LocalConfiguration.current.screenHeightDp.dp
    Column(
        modifier = Modifier.fillMaxSize()
    ) {
        Spacer(modifier = Modifier.height(16.dp))
        Box(
            modifier = Modifier.fillMaxWidth()
        ) {
            // Previous month
            Button(
                onClick = { selectedMonth -= 1 },
                colors = ButtonDefaults.buttonColors(
                    containerColor = MaterialTheme.colorScheme.onPrimary,
                    contentColor = MaterialTheme.colorScheme.primary),
                shape = RoundedCornerShape(5.dp),
                modifier = Modifier
                    .width(100.dp)
                    .height(50.dp)
                    .border(
                        width = 1.dp,
                        color = MaterialTheme.colorScheme.primary,
                        shape = RoundedCornerShape(5.dp)
                    )
            ) {
                Text(text = "Prev")
            }

            // Next month
            Button(
                onClick = { selectedMonth += 1 },
                colors = ButtonDefaults.buttonColors(
                    containerColor = MaterialTheme.colorScheme.onPrimary,
                    contentColor = MaterialTheme.colorScheme.primary),
                shape = RoundedCornerShape(5.dp),
                modifier = Modifier
                    .align(Alignment.TopEnd)
                    .width(100.dp)
                    .height(50.dp)
                    .border(
                        width = 1.dp,
                        color = MaterialTheme.colorScheme.primary,
                        shape = RoundedCornerShape(5.dp)
                    )
            ) {
                Text(text = "Next")
            }

            // Column to display the year and month name
            Column(
                modifier = Modifier.align(Alignment.Center),
                horizontalAlignment = Alignment.CenterHorizontally
            ) {
                Text(text = "$selectedYear")
                Text(text = intToMonthName(selectedMonth))
            }
        }
        Spacer(modifier = Modifier.height(10.dp))
        //Check so if the the month becomes the earlier year or the next year
        if(selectedMonth==0){
            selectedMonth=11
            selectedYear-=1
        }
        if(selectedMonth==13){
            selectedMonth=1
            selectedYear+=1
        }

        //amount of days in the chosen month
        val amountDays = daysInMonth(selectedYear, selectedMonth)
        //week amount
        val daysPerWeek = 7
        //amount of weeks that month
        val weeks = amountDays / daysPerWeek
        // checking the "overflowing" days of the year if its not directly exact x amount of weeks
        val remainingDays = amountDays % daysPerWeek
        //making the print of calendar for each week
        for (week in 0 until weeks) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween
            ) {
                for (dayOfWeek in 1..daysPerWeek) {
                    val dayDate = week * daysPerWeek + dayOfWeek
                    val tempDateObject = DateInfo(dayDate, selectedMonth, selectedYear)
                    val calendarDateData = DataStorage.readingDates.find { it.theDate ==  tempDateObject}
                    var readData = 0
                    if (calendarDateData != null){
                        readData = calendarDateData.read
                    }
                    //box that contains date of the month and amount of read pages that day
                    Box(
                        modifier = Modifier
                            .width(screenWidth / 7)
                            .border(1.dp, Color.Black, shape = RoundedCornerShape(4.dp))
                            .clip(RoundedCornerShape(4.dp))
                            .background(
                                if (readData >= DataStorage.pageGoal.intValue) {
                                    Color(0xFF17CC17)
                                } else {
                                    MaterialTheme.colorScheme.errorContainer
                                }
                            )
                    ) {
                        Column(
                            modifier = Modifier,
                            verticalArrangement = Arrangement.Center,
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Text(
                                text = dayDate.toString(),
                                textAlign = TextAlign.Center,
                                fontSize = 20.sp,
                                color = MaterialTheme.colorScheme.primary
                            )

                            Text(
                                text = readData.toString(),
                                textAlign = TextAlign.Center,
                                fontSize = 16.sp,
                                color = MaterialTheme.colorScheme.primary
                            )
                        }
                    }

                }
            }
        }
        //if its some of the remainDays if it was not exactly x amount of weeks
        if (remainingDays > 0) {
            Row(
                horizontalArrangement = Arrangement.SpaceBetween
            ) {
                for (dayDate in 1..remainingDays) {
                    val tempDateObject = DateInfo(amountDays-remainingDays+dayDate, selectedMonth, selectedYear)
                    val calendarDateData = DataStorage.readingDates.find { it.theDate ==  tempDateObject}
                    var readData = 0 // base value if the if isn't true
                    if (calendarDateData != null){
                        readData = calendarDateData.read
                    }
                    //box that contains date of the month and amount of read pages that day
                    Box(
                        modifier = Modifier
                            .width(screenWidth / 7)
                            .border(1.dp, Color.Black, shape = RoundedCornerShape(4.dp))
                            .clip(RoundedCornerShape(4.dp))
                            .background(
                                if (readData >= DataStorage.pageGoal.intValue) {
                                    Color(0xFF17CC17)
                                } else {
                                    MaterialTheme.colorScheme.errorContainer
                                }
                            )
                    ) {
                        Column(
                            modifier = Modifier,
                            verticalArrangement = Arrangement.Center,
                            horizontalAlignment = Alignment.CenterHorizontally
                        ) {
                            Text(
                                //that math equation since it finds out out the start date for this
                                //"new" text then add the day of this "week" to it
                                text = (amountDays-remainingDays+dayDate).toString(),
                                textAlign = TextAlign.Center,
                                fontSize = 20.sp,
                                color = MaterialTheme.colorScheme.primary
                            )

                            Text(
                                text = readData.toString(),
                                textAlign = TextAlign.Center,
                                fontSize = 16.sp,
                                color = MaterialTheme.colorScheme.primary
                            )
                        }

                    }
                }
            }
        }
        Spacer(modifier = Modifier.weight(1f))
        //navigation back to homepage
        Row(
            horizontalArrangement = Arrangement.SpaceAround,
            modifier = Modifier
                .fillMaxWidth()
                .padding(top = 5.dp, bottom = 5.dp)
        ){
            NavButtons(
                homeCallback = onNavigateToHome,
                buttonText = NavigationBack
            )
        }
    }
}

//translator to get which month based of the month int value
fun intToMonthName(month: Int): String {
    return when (month) {
        1 -> "January"
        2 -> "February"
        3 -> "March"
        4 -> "April"
        5 -> "May"
        6 -> "June"
        7 -> "July"
        8 -> "August"
        9 -> "September"
        10 -> "October"
        11 -> "November"
        12 -> "December"
        else -> "Invalid Month"
    }
}

//convert year and month to amount of days that month has
fun daysInMonth(year: Int, month: Int): Int {
    return when (month) {
        1, 3, 5, 7, 8, 10, 12 -> 31
        4, 6, 9, 11 -> 30
        2 -> if (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)) 29 else 28
        else -> throw IllegalArgumentException("Invalid month")
    }
}
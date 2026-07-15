package com.example.inkreader

import android.widget.Toast
import androidx.compose.foundation.Image
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.wrapContentWidth
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.material3.TextField
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.ColorFilter
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.unit.dp
//composable for adding a book to the overview
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun AddBookScreen(
    onNavigateToHome: () -> Unit
) {
    val context = LocalContext.current
    //mutable variable for text field showing
    var name by remember { mutableStateOf("") }
    var page by remember { mutableIntStateOf(0) }
    //variable set to 0 just to set it in as a variable to show how many pages the user
    //have read, since its a new book its 0
    val start = 0
    Column(
        modifier = Modifier
            .fillMaxSize()
            .wrapContentWidth(Alignment.CenterHorizontally), //center in the middle of the width
    ) {
        Spacer(modifier = Modifier.height(20.dp))
        Row {
            //text field for book name, 0.5f fill to take up half the width of screen
            TextField(
                value = name,
                onValueChange = { name = it },
                label = { Text("Book Name") },
                modifier = Modifier
                    .fillMaxWidth(0.5f)
                    .padding(16.dp)
            )
            //text field for pages, nothing on fill value to take up rest of width
            //try and catch to make sure the app doesn't crash from deleting of number or more
            TextField(
                value = page.toString(),
                onValueChange = { newText ->
                    try {
                        var newPage = newText
                        if (newPage.toInt() >= 0) { // Ensure it's a non-negative number
                            newPage = if (newPage == "0") {
                                newPage.removePrefix("0").removeSuffix("0")
                            } else {
                                newPage // Update the displayed value
                            }
                            page = newPage.toInt()
                        }
                    } catch (e: NumberFormatException) {
                        page = 0
                    }
                },
                label = { Text("Pages") },
                keyboardOptions = KeyboardOptions.Default.copy(
                    keyboardType = KeyboardType.Number
                ),
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(16.dp)
            )
        }
        Spacer(modifier = Modifier.weight(1f))
        Row( modifier = Modifier
            .padding(5.dp)
            .fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceAround){
            //add to Books the mutable list when pressed, resets the text field values
            Button(
                onClick = {
                    if(name == "" || page == 0){
                        Toast.makeText(context, "Failed adding book", Toast.LENGTH_SHORT).show()
                    }else {
                        val bookItem = BooksInfo(name, page, start)
                        DataStorage.Books.add(bookItem)
                        Toast.makeText(context, "Book Added", Toast.LENGTH_SHORT).show()
                    }
                    name = ""
                    page = 0

                },
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
            ) {
                saveDataToStorage(context)
                Text(text = "Add")
            }
            NavButtons(
                    homeCallback = onNavigateToHome,
            buttonText = NavigationBack
            )
        }
    }
    //background image of a cool looking octopus, as the page was somewhat empty
    Image(
        colorFilter = ColorFilter.tint(MaterialTheme.colorScheme.onPrimary),
        painter = painterResource(id = R.drawable.octopus),
        contentDescription = null,
        modifier = Modifier
            .fillMaxSize()
    )

}
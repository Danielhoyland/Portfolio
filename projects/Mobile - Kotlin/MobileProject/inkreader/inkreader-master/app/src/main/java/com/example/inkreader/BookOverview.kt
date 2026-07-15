package com.example.inkreader

import android.widget.Toast
import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.material3.TextField
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier

import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
//Composable for the overview screen
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun BookOverviewScreen(
    onNavigateToHome: () -> Unit
) {
    //remember variables, to update recompose if they are changed
    var showAlert by remember { mutableStateOf(false) }
    var showAlertBool by remember { mutableStateOf(false) }

    //screen width value
    val screenWidth = LocalConfiguration.current.screenWidthDp.dp
    //scroll state
    val scrollState = rememberScrollState()
    //edit mode or not value
    var isEditing by remember { mutableStateOf(false) }
    var indexDelete by remember { mutableStateOf(0) }
    var deleteBool by remember { mutableStateOf(false) }
    val context = LocalContext.current
    saveDataToStorage(context)

    if(deleteBool && (showAlertBool || DataStorage.showAlertMessage.value)){
        DataStorage.Books.removeAt(indexDelete)
        deleteBool = false
        showAlertBool = false
        Toast.makeText(context, "Book Deleted", Toast.LENGTH_SHORT).show()
    }

    if (showAlert && !DataStorage.showAlertMessage.value) {
        AlertDialog(
            onDismissRequest = {
                // Handle dismiss if needed
                showAlert = false
            },
            title = {
                Text(text = "Did you mean to delete?")
            },
            text = {
                Text(text = "Confirm to delete the book")
            },
            confirmButton = {
                Button(
                    onClick = {
                        showAlert = false
                        showAlertBool = true
                    }
                ) {
                    Text("DELETE")
                }
                Button(
                    onClick = {
                        showAlert = false
                    }
                ) {
                    Text("CANCEL")
                }
            }
        )
    }else{
        showAlert = false
    }
    Column(
        verticalArrangement = Arrangement.Top,
        modifier = Modifier
            .fillMaxWidth(1f)
            .verticalScroll(state = scrollState)
    ) {
        // Display a list of books
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .background(color = MaterialTheme.colorScheme.primaryContainer)
                .padding(8.dp)
        ) {
            //3 values so they get 1/3 of the width
            Text(text = "Name of Book",
                color = MaterialTheme.colorScheme.onPrimaryContainer,
                modifier = Modifier
                    .width(screenWidth/4))
            Text(text = "Amount of pages",
                modifier = Modifier
                    .width(screenWidth/4))
            Text(text = "Current Page",
                modifier = Modifier
                    .width(screenWidth/4))
        }
        //go through every book added and adding them to the screen
        DataStorage.Books.forEachIndexed { index, book ->
            if(!DataStorage.hideFinishedBooks.value || book.numberOfSides!=book.pageOn) {
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(8.dp)
                ) {
                    // Name of the book
                    if (isEditing) {
                        // Edit mode: Use a TextField
                        //using mutable variable to try not to catch any errors
                        var editedBookName by remember { mutableStateOf(book.bookName) }
                        TextField(
                            value = editedBookName,
                            onValueChange = {
                                val newText = it
                                book.bookName = newText
                                editedBookName = newText
                            },
                            label = { Text("Book Name") },
                            modifier = Modifier.width(screenWidth / 4) //1/3 of the screen since 3 values
                        )
                    } else {
                        // View mode: Display the book name
                        Text(text = book.bookName, modifier = Modifier.width(screenWidth / 4))
                    }

                    // Number of pages
                    if (isEditing) {
                        // Edit mode
                        //using mutable variable to try not to catch any errors
                        var editedNumberOfSides by remember { mutableStateOf(book.numberOfSides.toString()) }
                        TextField(
                            value = editedNumberOfSides,
                            onValueChange = {
                                val newText = it
                                try {
                                    var amountSide = newText
                                    if (amountSide.toInt() >= 0) {
                                        amountSide = if (amountSide == "0") {
                                            it.removePrefix("0").removeSuffix("0")
                                        } else {
                                            it // Update the displayed value
                                        }
                                        book.numberOfSides = amountSide.toInt()
                                        editedNumberOfSides = newText // Update the displayed value
                                    }

                                } catch (e: NumberFormatException) {
                                    editedNumberOfSides = "0" // Reset to 0 if not a valid number
                                    book.numberOfSides = 0
                                }
                            },
                            label = { Text("Pages") },
                            keyboardOptions = KeyboardOptions.Default.copy(keyboardType = KeyboardType.Number),
                            modifier = Modifier.width(screenWidth / 4)
                        )
                    } else {
                        // View mode: Display the number of pages
                        Text(
                            text = book.numberOfSides.toString(),
                            modifier = Modifier.width(screenWidth / 4)
                        )
                    }
                    // Initialize currentPageOn as a string
                    var currentPageOn by remember { mutableStateOf(book.pageOn.toString()) }
                    //text field to change the page in book the user is on
                    TextField(
                        value = currentPageOn,
                        onValueChange = {
                            try {
                                if (it.toInt() in 0..book.numberOfSides) {
                                    currentPageOn = if (currentPageOn == "0") {
                                        it.removePrefix("0").removeSuffix("0")
                                    } else {
                                        it // Update the displayed value
                                    }
                                }else if(it.toInt()>book.numberOfSides){
                                    currentPageOn = book.numberOfSides.toString()
                                }
                            } catch (e: NumberFormatException) {
                                currentPageOn = "0" // Reset to 0 if not a valid number
                            }
                        },
                        label = { Text("Page On") },
                        keyboardOptions = KeyboardOptions.Default.copy(keyboardType = KeyboardType.Number),
                        modifier = Modifier.width(screenWidth / 4)
                    )
                    Column {
                        Button(
                            onClick = {
                                val pageOnSide = currentPageOn.toInt()

                                val existingDate =
                                    DataStorage.readingDates.find { it.theDate == DataStorage.currentDateObject }
                                if (existingDate == null) {
                                    // If the current date is not in the list, add it with 0 on the 'read' field
                                    val newDate = PagesReadDates(
                                        theDate = DataStorage.currentDateObject,
                                        read = (pageOnSide - book.pageOn)
                                    )
                                    DataStorage.readingDates.add(newDate)

                                } else {
                                    existingDate.read += (pageOnSide - book.pageOn)

                                }
                                // Update book.pageOn only when the value is valid
                                book.pageOn = pageOnSide
                                Toast.makeText(context, "Edits saved", Toast.LENGTH_SHORT).show()
                            },
                            modifier = Modifier
                                .width(screenWidth / 4)
                                .height(28.dp),
                            colors = ButtonDefaults.buttonColors(
                                containerColor = MaterialTheme.colorScheme.onPrimary,
                                contentColor = MaterialTheme.colorScheme.primary),
                        ) {
                            Text(text = "✓", fontSize = 10.sp)
                        }

                        Button(
                            onClick = {
                                indexDelete = index
                                deleteBool = true
                                showAlert = true
                            },
                            modifier = Modifier
                                .width(screenWidth / 4)
                                .height(28.dp),
                            colors = ButtonDefaults.buttonColors(
                                containerColor = Color.Red, // Set the background color to red
                                contentColor = MaterialTheme.colorScheme.primary // Text color
                            )
                        ) {
                            Text(text = "✖", fontSize = 10.sp)

                        }
                    }
                }
            }
        }
        Spacer(modifier = Modifier.height(60.dp))
    }
    Column(
    ) {
        Spacer(modifier = Modifier.weight(1f))
        // --------- NAVBUTTONS
        Row(
            horizontalArrangement = Arrangement.SpaceAround,
            modifier = Modifier
                .fillMaxWidth()
                .padding(bottom = 4.dp)
        ) {
            //edit button to edit book title or amount of pages
            //button text changes based on mode
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
                    contentColor = MaterialTheme.colorScheme.primary
                ),
                shape = RoundedCornerShape(5.dp),
                onClick = {
                    // Toggle edit mode for all books
                    isEditing = !isEditing
                    if (isEditing) Toast.makeText(
                        context,
                        "Edit mode activated",
                        Toast.LENGTH_SHORT
                    ).show()
                    else Toast.makeText(context, "Edit mode deactivated", Toast.LENGTH_SHORT).show()

                }
            ) {
                Text(text = if (isEditing) "Stop Edit All" else "Edit All")
            }
            //back to homepage button
            NavButtons(
                homeCallback = onNavigateToHome,
                buttonText = NavigationBack
            )

        }
    }
}
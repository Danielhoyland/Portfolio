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
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.material3.TextField
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue

import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.unit.dp

//Settings screen composable
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SettingsScreen(
    onNavigateToHome: () -> Unit
) {
    var showAlert by remember { mutableStateOf(false) } //show an alert if needed (deleting data)
    if (showAlert) {
        AlertDialog(
            onDismissRequest = {
                // Handle dismiss if needed
                showAlert = false
            },
            title = {
                Text(text = "ARE YOU SURE TO DELETE ALL DATA")
            },
            text = {
                Text(text = "Confirm to delete all data")
            },
            confirmButton = {
                Button(
                    onClick = {
                        DataStorage.readingDates.clear()
                        DataStorage.Books.clear()
                        DataStorage.pageGoal.intValue = 1
                        DataStorage.isDarkModeEnabled.value = false
                        DataStorage.showAlertMessage.value = false
                        DataStorage.hideFinishedBooks.value = false
                        showAlert = false
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
    }
    Column(
        horizontalAlignment = Alignment.CenterHorizontally,
    ) {
        //Setting page goal
        Spacer(modifier = Modifier.height(16.dp))
        var tempValue = DataStorage.pageGoal.intValue.toString()
        TextField(value = tempValue, onValueChange = {
            try {
                tempValue = if (tempValue == "0") {
                    it.removePrefix("0")
                } else {
                    it // Update the displayed value
                }
                DataStorage.pageGoal.intValue = tempValue.toInt()
            } catch (e: NumberFormatException) {
                tempValue = "0" // Reset to 0 if not a valid number
                DataStorage.pageGoal.intValue = tempValue.toInt()
            }
        },
            label = { Text("Page Goal") },
            keyboardOptions = KeyboardOptions.Default.copy(keyboardType = KeyboardType.Number) )
        Spacer(modifier = Modifier.height(16.dp))
        val context = LocalContext.current
        Row(horizontalArrangement = Arrangement.Center,
            verticalAlignment = Alignment.CenterVertically,
            modifier = Modifier
                .fillMaxWidth(1.0f)
                .background(
                    color = MaterialTheme.colorScheme.primaryContainer,
                    shape = MaterialTheme.shapes.medium
                )
        ){
            //Setting for enabling dark mode
            Text(
                text = "Dark-mode",
                modifier = Modifier
                    .align( Alignment.CenterVertically)
            )
            saveDataToStorage(context)
            Switch(checked = DataStorage.isDarkModeEnabled.value, onCheckedChange = {
                DataStorage.isDarkModeEnabled.value = !DataStorage.isDarkModeEnabled.value
                if(DataStorage.isDarkModeEnabled.value) Toast.makeText(context, "Dark mode activated", Toast.LENGTH_SHORT).show()
                else Toast.makeText(context, "Dark mode deactivated", Toast.LENGTH_SHORT).show()
            })
        }
        Spacer(modifier = Modifier.height(8.dp))
        Row(horizontalArrangement = Arrangement.Center,
            verticalAlignment = Alignment.CenterVertically,
            modifier = Modifier
            .fillMaxWidth(1.0f)
            .background(
                color = MaterialTheme.colorScheme.primaryContainer,
                shape = MaterialTheme.shapes.medium
            )){
            //Hiding delete book alert (setting)
            Text(text = "Hide Delete Alert",
                modifier = Modifier
                    .align( Alignment.CenterVertically)
            )
            Switch(checked = DataStorage.showAlertMessage.value, onCheckedChange = {
                DataStorage.showAlertMessage.value = !DataStorage.showAlertMessage.value
                if(DataStorage.showAlertMessage.value) Toast.makeText(context, "Hiding Delete Alert", Toast.LENGTH_SHORT).show()
                else Toast.makeText(context, "Stopping hiding Delete Alert", Toast.LENGTH_SHORT).show()
            })
        }
        Spacer(modifier = Modifier.height(8.dp))
        Row(horizontalArrangement = Arrangement.Center,
            verticalAlignment = Alignment.CenterVertically,
            modifier = Modifier
            .fillMaxWidth(1.0f)
            .background(
                color = MaterialTheme.colorScheme.primaryContainer,
                shape = MaterialTheme.shapes.medium
            )){//Setting for hiding books that is finished
            Text(text = "Hide Finished Books",
                modifier = Modifier
                    .align( Alignment.CenterVertically))
            Switch(checked = DataStorage.hideFinishedBooks.value, onCheckedChange = {
                DataStorage.hideFinishedBooks.value = !DataStorage.hideFinishedBooks.value
                if(DataStorage.hideFinishedBooks.value) Toast.makeText(context, "Hiding Finished Books", Toast.LENGTH_SHORT).show()
                else Toast.makeText(context, "Stopping hiding Finished Books", Toast.LENGTH_SHORT).show()
            })
        }
        Spacer(modifier = Modifier.weight(1f))
        Row(
            modifier = Modifier
                .padding(5.dp)
                .fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceAround
        ){//button for deleting all data
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
                onClick = {
                    showAlert = true
                }) {
                Text(text = "DELETE ALL DATA")
            }
            NavButtons( //button for navigating back to the homepage
                homeCallback = onNavigateToHome,
                buttonText = NavigationBack
            )
        }
        }


}
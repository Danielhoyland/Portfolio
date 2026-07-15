package com.example.inkreader

import android.content.Context
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import java.util.Calendar

object DataStorage {
    var Books: MutableList<BooksInfo> = mutableListOf()

    //make the calendar
    private val calendar = Calendar.getInstance()
    // Extract month, day, and year components
    val year = calendar.get(Calendar.YEAR)
    val month = calendar.get(Calendar.MONTH) + 1 // Adding 1 to adjust for 0-based months
    val day = calendar.get(Calendar.DAY_OF_MONTH)

    //What the current day is
    val currentDateObject = DateInfo(day, month, year)

    //Settings and storage for dates where you have read something
    var readingDates: MutableList<PagesReadDates> = mutableListOf()
    var pageGoal = mutableIntStateOf(1)
    var isDarkModeEnabled = mutableStateOf(false)
    var showAlertMessage = mutableStateOf(false)
    var hideFinishedBooks = mutableStateOf(false)
}

//Info on each of the books
data class BooksInfo(var bookName: String, var numberOfSides: Int, var pageOn: Int)

//What dates how many pages were read
data class PagesReadDates(var theDate: DateInfo, var read: Int)

//How dates are formatted
data class DateInfo(val day: Int, val month: Int, val year: Int)

//storage of the data and retrieval
fun saveDataToStorage(context: Context) {
    val sharedPreferences = context.getSharedPreferences("InkReaderPrefs", Context.MODE_PRIVATE)
    val editor = sharedPreferences.edit()

    val booksJson = Gson().toJson(DataStorage.Books)
    val datesJson = Gson().toJson(DataStorage.readingDates)
    val pageGoalJson = Gson().toJson(DataStorage.pageGoal.intValue)
    val isDarkModeEnabledJson = Gson().toJson(DataStorage.isDarkModeEnabled.value)
    val isShowAlertMessageEnabledJson = Gson().toJson(DataStorage.showAlertMessage.value)
    val isHideFinishedBooksEnabledJson = Gson().toJson(DataStorage.hideFinishedBooks.value)

    editor.putString("books_data", booksJson)
    editor.putString("dates_data", datesJson)
    editor.putString("page_goal", pageGoalJson)
    editor.putString("dark_mode_enabled", isDarkModeEnabledJson)
    editor.putString("show_alert_message_enabled", isShowAlertMessageEnabledJson)
    editor.putString("hide_finished_books_enabled", isHideFinishedBooksEnabledJson)
    editor.apply()
}

fun loadDataFromStorage(context: Context) {
    val sharedPreferences = context.getSharedPreferences("InkReaderPrefs", Context.MODE_PRIVATE)

    val booksJson = sharedPreferences.getString("books_data", null)
    val datesJson = sharedPreferences.getString("dates_data", null)
    val pageGoalJson = sharedPreferences.getString("page_goal", null)
    val isDarkModeEnabledJson = sharedPreferences.getString("dark_mode_enabled", null)
    val isShowAlertMessageEnabledJson = sharedPreferences.getString("show_alert_message_enabled", null)
    val isHideFinishedBooksEnabledJson = sharedPreferences.getString("hide_finished_books_enabled", null)


    if (booksJson != null) {
        DataStorage.Books = Gson().fromJson(booksJson, object : TypeToken<MutableList<BooksInfo>>() {}.type)
    }

    if (datesJson != null) {
        DataStorage.readingDates = Gson().fromJson(datesJson, object : TypeToken<MutableList<PagesReadDates>>() {}.type)
    }

    if (pageGoalJson != null) {
        DataStorage.pageGoal.intValue = Gson().fromJson(pageGoalJson, Int::class.java)
    }

    if (isDarkModeEnabledJson != null) {
        DataStorage.isDarkModeEnabled.value = Gson().fromJson(isDarkModeEnabledJson, Boolean::class.java)
    }

    if (isShowAlertMessageEnabledJson != null) {
        DataStorage.showAlertMessage.value = Gson().fromJson(isShowAlertMessageEnabledJson, Boolean::class.java)
    }

    if (isHideFinishedBooksEnabledJson != null) {
        DataStorage.hideFinishedBooks.value = Gson().fromJson(isHideFinishedBooksEnabledJson, Boolean::class.java)
    }
}

var NavigationBack: String = "Back"
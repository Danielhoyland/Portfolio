using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

/**
 * Stopwatch
 */
public class Stopwatch : MonoBehaviour
{
    [SerializeField] private TMP_Text stopwatchUI;
    private bool usesUI;
    private float currentTime;
    private bool isRunning;


    private void Start()
    {
        usesUI = stopwatchUI != null;
    }

    private void Update()
    {
        if (isRunning)
        {
            currentTime += Time.unscaledDeltaTime; //Time.deltaTime / Time.timeScale;
            
            if (usesUI) stopwatchUI.text = FormatTime(currentTime);
        }
    }

    public void StartStopwatch() { isRunning = true; }
    public void StopStopwatch() { isRunning = false; }
    public void ResetStopwatch() { currentTime = 0f; }
    public float GetTime() => currentTime;


    public static string FormatTime(float time)
    {
        if (time < 0f) return "--:--.---";

        //int hours      = Mathf.FloorToInt(time) / 3600;
        int minutes      = Mathf.FloorToInt(time) / 60;
        int seconds      = Mathf.FloorToInt(time) - minutes * 60;
        int milliseconds = Mathf.FloorToInt((time - Mathf.FloorToInt(time)) * 1000);

        string minutesString      = minutes < 10 ? "0" + minutes.ToString() : minutes.ToString();
        string secondsString      = seconds < 10 ? "0" + seconds.ToString() : seconds.ToString();
        string millisecondsString = milliseconds.ToString();

        while (millisecondsString.Length < 3) millisecondsString = "0" + millisecondsString;

        return minutesString + ":" + secondsString + "." + millisecondsString;
    }
}

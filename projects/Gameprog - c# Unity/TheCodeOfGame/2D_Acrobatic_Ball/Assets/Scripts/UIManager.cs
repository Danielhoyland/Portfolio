using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;
using UnityEngine.UI;

public class UIManager : MonoBehaviour
{
    [Header("Start Panel")]
    [SerializeField] private GameObject startPanel;
    [SerializeField] private TMP_Text timeToBeatUI;
    [SerializeField] private TMP_Text levelNameUI;

    [Header("Game HUD")]
    [SerializeField] private GameObject gameHUD;

    [Header("Death Panel")]
    [SerializeField] private GameObject deathPanel;

    [Header("Pause")]
    [SerializeField] private GameObject pausePanel;

    [Header("Settings")]
    [SerializeField] private GameObject settingsPanel;

    [Header("KeyBindingPage")]
    [SerializeField] private GameObject keyBindPanel;

    [Header("Win Panel")]
    [SerializeField] private GameObject winPanel;
    [SerializeField] private TMP_Text winLevelTimeUI;
    [SerializeField] private TMP_Text winBestTimeUI;
    [SerializeField] private Image winStarsImageUI;
    [SerializeField] private GameObject winNextStarUI;
    [SerializeField] private TMP_Text winNextStarTimeUI;
    [SerializeField] private Image winNextStarImageUI;

    public void ToggleDeathPanel()
    {
        deathPanel.SetActive(!deathPanel.activeSelf);
    }

    public void ToggleWinPanel()
    {
        winPanel.SetActive(!winPanel.activeSelf);
    }
    public void TogglePausePanel()
    {
        pausePanel.SetActive(!pausePanel.activeSelf);
    }
    public void ToggleSettingsPanel()
    {
        settingsPanel.SetActive(!settingsPanel.activeSelf);
    }
    public void ToggleKeyBindPanel()
    {
        keyBindPanel.SetActive(!keyBindPanel.activeSelf);
    }
    public bool TogglePausePanelBool()
    {
        return pausePanel.activeSelf;
    }
    public bool ToggleSettingsPanelBool()
    {
        return settingsPanel.activeSelf;
    }
    public bool ToggleKeyBindPanelBool()
    {
        return keyBindPanel.activeSelf;
    }

    public GameObject KeyBindButton()
    {
        return settingsPanel.transform.Find("CHANGEKEYBIND").gameObject;
    }

    public void SetWinPanelTimes(string levelTime, string bestTime, bool newBest)
    {
        winLevelTimeUI.text = levelTime;
        winBestTimeUI.text = bestTime;

        Color red = new Color32(182,0,0,255);
        Color green = new Color32(8,142,0,255);
        winLevelTimeUI.color = newBest ? green : red;
    }
    public void SetWinPanelStars(int maxStarCount, int starCount, Sprite stars, string nextStarTimeRequirement, Sprite nextStar)
    {
        winStarsImageUI.sprite = stars;

        if (starCount < maxStarCount)
        {
            winNextStarTimeUI.text = nextStarTimeRequirement;
            winNextStarImageUI.sprite = nextStar;
        }
        else
        {
            winNextStarUI.SetActive(false);
        }
    }

    public void ShowGameHUD() { gameHUD.SetActive(true); }
    public void HideGameHUD() { gameHUD.SetActive(false); }

    public void ShowStartPanel() { startPanel.SetActive(true); }
    public void HideStartPanel() { startPanel.SetActive(false); }
    public void PrepareStartPanel(string levelName, string timeToBeat)
    {
        levelNameUI.text = levelName;
        timeToBeatUI.text = timeToBeat;
    }
}

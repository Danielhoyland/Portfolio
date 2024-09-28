using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.InputSystem.UI;
using UnityEngine.UI;

public class MainMenuManager : MonoBehaviour
{
    [Header("Title Screen")]
    [SerializeField] private GameObject titleScreen;

    [Header("Title Screen")]
    [SerializeField] private GameObject levelSelectScreen;
    [SerializeField] private Sceneloader levelList;

    [Header("Title Screen")]
    [SerializeField] private GameObject settingsScreen;


    private void Start()
    {
        Time.timeScale = 1.0f;
    }


    #region TitleScreen

    /**
     * Opens the title screen / main menu
     */
    public void OpenTitleScreen()
    {
        levelSelectScreen.SetActive(false);
        settingsScreen.SetActive(false);

        titleScreen.SetActive(true);
    }

    /**
     * Quits the game/application
     */
    public void QuitGame()
    {
        #if UNITY_EDITOR
        UnityEditor.EditorApplication.isPlaying = false;
        #endif

        Application.Quit();
    }

    #endregion


    #region LevelSelectScreen

    /**
     * Opens the title screen / main menu
     */
    public void OpenLevelSelectScreen()
    {
        titleScreen.SetActive(false);
        settingsScreen.SetActive(false);

        levelSelectScreen.SetActive(true);

        levelList.AddLevelButtons();
    }

    #endregion


    #region SettingsScreen

    /**
     * Opens the settings screen
     */
    public void OpenSettingsScreen()
    {
        titleScreen.SetActive(false);
        levelSelectScreen.SetActive(false);

        settingsScreen.SetActive(true);
    }

    /**
     * OnClick function for resetting the data in the level data save file
     */
    public void ResetSaveFile()
    {
        LevelDataIO.ResetSaveFile();
        StartCoroutine(ResetButtonColor());
    }

    /**
     * Sets the color of the currently selected button to the normal color after a given time
     */
    private IEnumerator ResetButtonColor(float waitTime = 0.1f)
    {
        yield return new WaitForSeconds(waitTime);
        EventSystem.current.SetSelectedGameObject(null);
    }

    #endregion
}

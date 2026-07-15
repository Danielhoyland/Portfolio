using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.InputSystem;

public class LevelManager : MonoBehaviour
{
    public static LevelManager instance;

    private bool isPlaying = false;
    public bool isPaused = false;
    public bool isDeath = false;
    public bool isWin = false;

    private LevelDataManager levelDataManager;
    private LevelScore levelScore;
    [SerializeField] private Stopwatch stopwatch;


    private void Awake()
    {
        if (LevelManager.instance == null) instance = this;
        else Destroy(gameObject);
    }

    private void Start()
    {
        levelDataManager = GetComponent<LevelDataManager>();
        levelScore = GetComponent<LevelScore>();

        PrepareLevel();
    }


    public void PrepareLevel()
    {
        Time.timeScale = 0f;
        stopwatch.ResetStopwatch();

        UIManager _ui = GetComponent<UIManager>();
        if (_ui != null)
        {
            _ui.PrepareStartPanel(SceneManager.GetActiveScene().name, Stopwatch.FormatTime(levelDataManager.GetBestTime()));
            _ui.ShowStartPanel();
        }
    }

    public void StartLevel()
    {
        Time.timeScale = 1f;

        UIManager _ui = GetComponent<UIManager>();
        if (_ui != null)
        {
            _ui.HideStartPanel();
            _ui.ShowGameHUD();
        }
        stopwatch.StartStopwatch();
    }

    private void OnAnyInput()
    {
        if (!isPlaying)
        {
            isPlaying = true;
            StartLevel();
        }
    }


    public void GameOver()
    {
        Time.timeScale = 0f;
        stopwatch.StartStopwatch();

        GameObject.Find("Player").GetComponent<PlayerStateMachine>().PlayerDeath();

        UIManager _ui = GetComponent<UIManager>();
        if(_ui != null)
        {
            _ui.HideGameHUD();
            _ui.ToggleDeathPanel();
        }
    }

    public void Win()
    {
        Time.timeScale = 0f;

        stopwatch.StopStopwatch();

        UIManager _ui = GetComponent<UIManager>();
        if(_ui != null)
        {
            _ui.HideGameHUD();

            float levelTime = stopwatch.GetTime();
            float bestTime = levelDataManager.GetBestTime();
            bool newBest = levelTime < bestTime || bestTime < 0f;
            int starCount = levelScore.GetScore(levelTime);

            _ui.SetWinPanelTimes(Stopwatch.FormatTime(levelTime), Stopwatch.FormatTime(bestTime), newBest);
            _ui.SetWinPanelStars(
                LevelScore.MAX_SCORE, starCount,
                levelScore.GetStarSprite(starCount),
                Stopwatch.FormatTime(levelScore.GetTimeRequirement(starCount + 1)),
                levelScore.GetStarSprite(starCount + 1));

            _ui.ToggleWinPanel();

            levelDataManager.UpdateLevelScore(starCount);
        }
        ToggleWin();
        levelDataManager.UpdateTime(stopwatch.GetTime());
    }

    public void TogglePause()
    {
        if (Time.timeScale != 0f)
        {
            Time.timeScale = 0f;
            stopwatch.StopStopwatch();
        }
        else
        {
            Time.timeScale = 1f;
            stopwatch.StartStopwatch();
        }
        isPaused = !isPaused;
    }
    public void UnPause()
    {
        Time.timeScale = 1f;
    }
    public void ToggleWin() {
        isWin = !isWin;
    }
    public void ToggleDeath() {
        isDeath = !isDeath;
    }
}

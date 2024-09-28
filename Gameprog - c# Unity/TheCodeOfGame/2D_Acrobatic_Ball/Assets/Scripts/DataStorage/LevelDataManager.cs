using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.SceneManagement;

/**
 * Handles loading, updating and saving of level data
 */
public class LevelDataManager : MonoBehaviour
{
    private Dictionary<int, LevelData> levelData;

    [SerializeField] private Stopwatch stopwatch;


    private void Awake()
    {
        levelData = LevelDataIO.LoadSaveFileData();
    }


    /**
     * Updates the best time if it is beaten
     */
    public void UpdateTime(float newTime)
    {
        if (newTime < 0)
        {
            Debug.Log("Level was completed with a time less than 0 for some reason.");
            return;
        }

        if (levelData[SceneManager.GetActiveScene().buildIndex].SetBestTime(newTime)) LevelDataIO.SaveLevelData(levelData);
    }

    public void UpdateLevelScore(int starCount)
    {
        if (starCount > levelData[SceneManager.GetActiveScene().buildIndex].starCount) levelData[SceneManager.GetActiveScene().buildIndex].starCount = starCount;
    }


    public void LockLevel() { levelData[SceneManager.GetActiveScene().buildIndex].LockLevel(); }
    public void UnlockLevel() { levelData[SceneManager.GetActiveScene().buildIndex].UnlockLevel(); }
    public float GetBestTime() => levelData[SceneManager.GetActiveScene().buildIndex].bestTime;
    public LevelData GetLevelData() => levelData[SceneManager.GetActiveScene().buildIndex];
}

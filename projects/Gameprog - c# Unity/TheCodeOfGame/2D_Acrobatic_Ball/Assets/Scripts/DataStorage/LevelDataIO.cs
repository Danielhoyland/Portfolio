using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.SceneManagement;

public static class LevelDataIO
{
#if UNITY_EDITOR
    public static readonly string saveFilePath = Application.dataPath + Path.AltDirectorySeparatorChar + "SaveFiles/LevelData.txt";
#else
    public static readonly string saveFilePath = Application.persistentDataPath + Path.AltDirectorySeparatorChar + "LevelData.txt";
#endif


    #region Load

    /**
     * Reads the save file and returns the data
     */
    public static Dictionary<int,LevelData> LoadSaveFileData()
    {
        Dictionary<int,LevelData> levelData = new();

        if (File.Exists(saveFilePath))
        {
            StreamReader reader = new StreamReader(saveFilePath);
            string saveFileContent = reader.ReadToEnd();
            reader.Close();

            levelData = ParseLevelData(saveFileContent, saveFilePath);
        }
        else
        {
            Debug.Log("Could not find save file \"" + saveFilePath + "\". Creating save file now.");
            File.Create(saveFilePath).Dispose();
        }

        if (levelData.Count != SceneManager.sceneCountInBuildSettings) CreateData(ref levelData);

        return levelData;
    }

    /**
     * Parses level data from string to type Dictionary<int,LevelData>
     */
    private static Dictionary<int,LevelData> ParseLevelData(string text, string saveFilePath)
    {
        Dictionary<int,LevelData> levelData = new();

        if (text == null || text == "")
        {
            Debug.LogWarning("Save file " + saveFilePath + " is NULL or empty.");
            return levelData;
        }

        string[] levels = text.Split('\n');
        if (levels[^1] == "") levels = levels[..^1]; // The save file ends with "\n"

        foreach (string level in levels)
        {
            string[] data = level.Split(' ');
            if (data.Length != 4)
            {
                Debug.LogWarning("Level data for a level in save file is corrupted: " + level);
                continue;
            }

            if (int.TryParse(data[0], out int index) &&
                bool.TryParse(data[1], out bool isLocked) &&
                float.TryParse(data[2], out float time) && 
                int.TryParse(data[3], out int starCout))
            {
                levelData.Add(index, new LevelData(isLocked, time, starCout));
            }
            else Debug.LogError("Could not parse data for a level in save file " + saveFilePath);
        }

        return levelData;
    }

    /**
     * Creates missing levelData
     */
    private static void CreateData(ref Dictionary<int,LevelData> levelData)
    {
        int firstLevel = 1; // Index 0 = main menu

        for (int i = firstLevel; i < SceneManager.sceneCountInBuildSettings; i++)
        {
            if (!levelData.ContainsKey(i))
            {
                levelData.Add(i, new LevelData());
            }
        }
    }

    #endregion


    #region Save

    /**
     * Saves level data to disc
     */
    public static void SaveLevelData(Dictionary<int,LevelData> levelData)
    {
        string text = ConvertToString(levelData);

        StreamWriter writer = new(saveFilePath);
        writer.Write(text);
        writer.Flush();
        writer.Close();
    }

    /**
     * Converts level data to string in the correct format for the save file
     */
    private static string ConvertToString(Dictionary<int,LevelData> levelData)
    {
        string text = "";
        foreach (var item in levelData)
        {
            text += item.Key + " " + item.Value.isLocked + " " + item.Value.bestTime + " " + item.Value.starCount + "\n";
        }
        return text;
    }

    #endregion


    /**
     * Removes saved progress from the save file
     */
    public static void ResetSaveFile()
    {
        Dictionary<int,LevelData> levelData = new();

        for (int i = 1; i < SceneManager.sceneCountInBuildSettings; i++)
        {
            levelData[i] = new LevelData();
        }

        SaveLevelData(levelData);
    }
}

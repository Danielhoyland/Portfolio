using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.SceneManagement;

public class UIscript : MonoBehaviour
{

    public void PlayScene(int index)
    {
        SceneManager.LoadSceneAsync(index);
    }

    public void PlayMenu()
    {
        SceneManager.LoadSceneAsync(0);
    }

    public void ReloadScene()
    {
        SceneManager.LoadSceneAsync(SceneManager.GetActiveScene().buildIndex);
    }

    public void PlayNextScene()
    {
        if (SceneManager.GetActiveScene().buildIndex >= SceneManager.sceneCountInBuildSettings - 1)
        {
            PlayMenu();
            return;
        }

        PlayScene(SceneManager.GetActiveScene().buildIndex + 1);
    }
}

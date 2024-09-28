using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using TMPro;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

public class Sceneloader : MonoBehaviour
{
    [SerializeField] GameObject ButtonPrefab;
    [SerializeField] Transform ButtonRoot;

    private Dictionary<int, LevelData> levelData;


    public void AddLevelButtons()
    {
        DestroyLevelButtons();

        levelData = LevelDataIO.LoadSaveFileData();
        // minus 1 as this gets the count, not the max of the index
        int maxIndex = SceneManager.sceneCountInBuildSettings - 1;

        // starts at 1, as 0 in the build index is the main menu
        foreach (int i in Enumerable.Range(1, maxIndex))
        {
            var button = Instantiate(ButtonPrefab, Vector3.zero, Quaternion.identity, ButtonRoot);

            button.name = i.ToString();
            // button.GetComponentInChildren<TextMeshProUGUI>().text = Utils.GetSceneName(i);
            button.transform.Find("Name").GetComponent<TextMeshProUGUI>().text = Utils.GetSceneName(i);

            // Displays stars and PB only if the player has completed the level
            if (levelData[i].bestTime > 0)
            {
                button.transform.Find("PB").GetComponent<TextMeshProUGUI>().text = "PB: " + Stopwatch.FormatTime(levelData[i].bestTime);
                button.transform.Find("Stars").GetComponent<Image>().sprite = Utils.GetStarSprite(levelData[i].starCount);
            }
            else
            {
                button.transform.Find("PB").gameObject.SetActive(false);
                button.transform.Find("Stars").gameObject.SetActive(false);
            }

            button.GetComponent<Button>().onClick = new Button.ButtonClickedEvent();

            button.GetComponent<Button>().onClick.AddListener(() => { SceneManager.LoadScene(i); });
        }   
    }

    void OnButtonClick()
    {

    }

    private void DestroyLevelButtons()
    {
        for (int i = transform.childCount - 1; i >= 0; i--)
        {
            Destroy(transform.GetChild(i).gameObject);
        }
    }
}

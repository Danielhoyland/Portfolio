using UnityEngine;
using UnityEngine.UI;

/**
 * Sprite anmation for UI Images
 * 
 * Stolen from: https://gist.github.com/almirage/e9e4f447190371ee6ce9
 * ... and modified
 */
public class UISpriteAnimation : MonoBehaviour
{
    public Sprite[] sprites;
    public float framesPerSecond = 6f;
    public bool loop = true;
    public bool destroyOnEnd = false;
    public bool randomSpriteOrder = false;

    private int index = 0;
    private Image image;
    private float time = 0f;

    void Awake()
    {
        image = GetComponent<Image>();
    }

    void Update()
    {
        if (!loop && index >= sprites.Length) return;

        time += Time.deltaTime;

        if (time < 1f / framesPerSecond) return;

        if (randomSpriteOrder)
        {
            int randomIndex = Mathf.FloorToInt(Random.Range(0f, sprites.Length - 1f));
            if (randomIndex >= index) randomIndex += 1;
            image.sprite = sprites[randomIndex];
            index = randomIndex;
        }
        else
        {
            image.sprite = sprites[index];
            index++;

            if (index >= sprites.Length)
            {
                if (loop) index = 0;
                if (destroyOnEnd) Destroy(gameObject);
            }
        }

        time -= 1f / framesPerSecond;
    }
}

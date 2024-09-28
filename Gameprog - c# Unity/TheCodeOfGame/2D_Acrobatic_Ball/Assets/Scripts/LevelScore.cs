using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LevelScore : MonoBehaviour
{
    [Header("Values")]
    [SerializeField] private float oneStarTimeRequirement = -1f;
    [SerializeField] private float twoStarTimeRequirement = -1f;
    [SerializeField] private float threeStarTimeRequirement = -1f;

    [Header("Sprites")]
    [SerializeField] private Sprite zeroStarsSprite;
    [SerializeField] private Sprite oneStarsSprite;
    [SerializeField] private Sprite twoStarsSprite;
    [SerializeField] private Sprite threeStarsSprite;

    public const int MAX_SCORE = 3;

    private void Start()
    {
        if (threeStarTimeRequirement <= 0f)
            Debug.LogError("Time requirement for 3 stars should be longer than 0 seconds." + threeStarTimeRequirement);

        if (twoStarTimeRequirement <= threeStarTimeRequirement)
            Debug.LogError("Time requirement for 2 stars should be longer than time requirement for 3 stars.");

        if (oneStarTimeRequirement <= twoStarTimeRequirement)
            Debug.LogError("Time requirement for 1 star should be longer than time requirement 2 stars.");
    }

    public float GetTimeRequirement(int starCount)
    {
        return Mathf.Clamp(starCount, 1, MAX_SCORE) switch
        {
            1 => oneStarTimeRequirement,
            2 => twoStarTimeRequirement,
            3 => threeStarTimeRequirement,
            _ => -1,
        };
    }

    public int GetScore(float time)
    {
        if (time <= threeStarTimeRequirement) return 3;
        if (time <= twoStarTimeRequirement) return 2;
        if (time <= oneStarTimeRequirement) return 1;
        return 0;
    }

    public Sprite GetStarSprite(int starCount)
    {
        switch (starCount)
        {
            case 1: return oneStarsSprite;
            case 2: return twoStarsSprite;
            case 3: return threeStarsSprite;
            default: return zeroStarsSprite;

        }
    }
}

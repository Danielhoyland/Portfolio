using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LevelData
{
    public bool isLocked { get; private set; }
    public float bestTime { get; private set; }
    public int starCount;
    
    // TODO: List of levels unlocked by completing this one


    public LevelData()
    {
        isLocked = false;
        bestTime = -1f;
        starCount = 0;
    }
    public LevelData(bool isLocked, float bestTime, int starCount)
    {
        this.isLocked = isLocked;
        this.bestTime = bestTime;
        this.starCount = starCount;
    }


    public void LockLevel()
    {
        isLocked = true;
    }

    public void UnlockLevel()
    {
        isLocked = false;
    }

    /**
     * Returns true if time is updated
     */
    public bool SetBestTime(float newTime)
    {
        if (bestTime < 0f || newTime < bestTime)
        {
            bestTime = newTime;
            return true;
        }
        return false;
    }
}

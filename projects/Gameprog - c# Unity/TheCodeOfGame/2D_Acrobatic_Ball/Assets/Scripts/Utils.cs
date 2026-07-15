using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.SceneManagement;

/**
 * Static class for methods that could be usefull most places
 */
public static class Utils
{
    /**
     * Converts LayerMask.value to correct value for LayerMasks that are assigned in the inspector.
     * 
     * Layers assignet in the inspector have a diffrent value from ther equivalent in Collider2D objects.
     * To not have to use a string to get a layer: `LayerMask.NameToLayer("Platform")` (gives correct value).
     * 
     * Stolen from:
     * https://discussions.unity.com/t/layer-layermask-which-is-set-in-inspector/179105/7
     */
    public static int ToSingleLayer(LayerMask mask)
    {
        int value = mask.value;
        if (value == 0) return 0;  // Early out
        for (int l = 1; l < 32; l++)
            if ((value & (1 << l)) != 0) return l;  // Bitwise
        return -1;  // This line won't ever be reached but the compiler needs it
    }


    /**
     * Returns the name of a scene in build by index
     * 
     * Stolen from:
     * https://stackoverflow.com/questions/36071344/getting-scene-names-at-runtime-creating-scenes-at-runtime
     * https://learn.microsoft.com/en-us/dotnet/api/system.io.path.getfilenamewithoutextension?view=net-8.0
     */
    public static string GetSceneName(int index)
    {
        //Regex regex = new(@"([^/]*/)*([\w\d\-]*)\.unity");
        string path = SceneUtility.GetScenePathByBuildIndex(index);
        return System.IO.Path.GetFileNameWithoutExtension(path);
        //return regex.Replace(path, "$2");
    }

    /**
     * Returns the sprte with the right amout of stars
     * https://discussions.unity.com/t/how-to-load-sprite-dynamically-from-assets-in-unity/184733/2
     */
    public static Sprite GetStarSprite(int starCount)
    {
        string path = starCount switch
        {
            3 => "Sprites/Stars/3_stars",
            2 => "Sprites/Stars/2_stars",
            1 => "Sprites/Stars/1_stars",
            _ => "Sprites/Stars/0_stars",
        };
        return Resources.Load<Sprite>(path);
    }
}

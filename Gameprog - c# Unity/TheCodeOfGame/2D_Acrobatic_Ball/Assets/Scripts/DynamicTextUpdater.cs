using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using TMPro;
using UnityEngine.InputSystem;

public class DynamicTextUpdater : MonoBehaviour
{
    [SerializeField] private InputActionAsset inputActionAsset;
    private TMP_Text textMeshProText;

    [SerializeField] private string controlDescription = "Move";

    private void Awake()
    {
        textMeshProText = GetComponent<TMP_Text>();
    }

    private void Update()
    {
        if (textMeshProText != null && inputActionAsset != null)
        {

            InputActionMap playerControlsMap = inputActionAsset.FindActionMap("Player");
            InputAction controlAction = playerControlsMap.FindAction(controlDescription);

            if (controlAction != null)
            {
                string controlBindingPath = "";

                controlBindingPath = controlAction.bindings[0].effectivePath; 
                if(controlBindingPath == "Dpad")
                {
                    controlBindingPath = "WASD";
                }
                // Check if controlBindingPath contains '<'
                int startIndex = controlBindingPath.IndexOf('<');
                int endIndex = controlBindingPath.IndexOf('/');
                string modifiedPath = "";
                string modifiedDescription = "";
                if (startIndex != -1 && endIndex != -1)
                {
                    // Remove everything between the first '<' and '/'
                    modifiedPath = controlBindingPath.Remove(startIndex, endIndex+1);
                }
                else
                {
                    modifiedPath = controlBindingPath;
                }
                if(controlDescription == "StartAim")
                {
                    modifiedDescription = "Aim";
                }
                else if(controlDescription == "ActivateThrowOrTeleport")
                {
                    modifiedDescription = "Throw the ball or teleport to it";
                }
                else { 
                    modifiedDescription = controlDescription; 
                }


                textMeshProText.text = $"To {modifiedDescription} Press {modifiedPath}";
            }
            else
            {
                Debug.LogError($"Action with description '{controlDescription}' not found in the current control scheme.");
            }
        }
    }
}





using UnityEngine;
using UnityEngine.InputSystem;

public class Settingss : MonoBehaviour
{
    private GameObject yourButton;
    [SerializeField] private InputActionAsset inputActionAsset;

    private void Start()
    {
        inputActionAsset = GetComponent<PlayerInput>().actions;

        if (LevelManager.instance != null)
        {
            UIManager uiManager = LevelManager.instance.GetComponent<UIManager>();

            if (uiManager != null)
            {
                yourButton = uiManager.KeyBindButton();

                if (yourButton == null)
                {
                    Debug.LogError("KeyBindButton not found in UIManager.");
                }
            }
            else
            {
                Debug.LogError("UIManager not found on LevelManager.");
            }
        }
        else
        {
            Debug.LogError("LevelManager.instance is null.");
        }
    }

    private void Update()
    {
        if (inputActionAsset != null && yourButton != null)
        {
            string currentControlScheme = "Keyboard&Mouse"; // Default to keyboard
            if (Keyboard.current == null && Mouse.current == null)
            {
                // If there's no keyboard or mouse input, assume it's a gamepad
                currentControlScheme = "GamePad";
            }

            InputActionMap playerControlsMap = inputActionAsset.FindActionMap("Player");

            if (currentControlScheme == "Keyboard&Mouse")
            {
                yourButton.SetActive(true); // Show the button for keyboard
            }
            else if (currentControlScheme == "GamePad")
            {
                yourButton.SetActive(false); // Hide the button for gamepad
            }
        }
    }
}

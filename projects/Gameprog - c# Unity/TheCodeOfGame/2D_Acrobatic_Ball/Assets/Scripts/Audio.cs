using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Audio;
using UnityEngine.UI;

public class Audio : MonoBehaviour
{
    public AudioMixer masterMixer;  // Assign your Audio Mixer in the Inspector

    [SerializeField] private Slider masterVolumSlider;
    [SerializeField] private Slider sfxVolumSlider;
    [SerializeField] private Slider musicVolumSlider;

    public const string MASTER_VOLUME_KEY = "MasterVol";
    public const string SFX_VOLUME_KEY = "SFXVol";
    public const string MUSIC_VOLUME_KEY = "MusicVol";

    private void Start()
    {
        float masterVolumValue = PlayerPrefs.GetFloat(MASTER_VOLUME_KEY, 0f);
        float sfxVolumValue = PlayerPrefs.GetFloat(SFX_VOLUME_KEY, 0f);
        float musicVolumValue = PlayerPrefs.GetFloat(MUSIC_VOLUME_KEY, 0f);

        masterMixer.SetFloat(MASTER_VOLUME_KEY, masterVolumValue);
        masterMixer.SetFloat(SFX_VOLUME_KEY, sfxVolumValue);
        masterMixer.SetFloat(MUSIC_VOLUME_KEY, musicVolumValue);

        masterVolumSlider.value = masterVolumValue;
        sfxVolumSlider.value = sfxVolumValue;
        musicVolumSlider.value = musicVolumValue;
    }

    public void SetMasterVolume(float masterVol)
    {
        masterMixer.SetFloat(MASTER_VOLUME_KEY, masterVol);
        PlayerPrefs.SetFloat(MASTER_VOLUME_KEY, masterVol);
    }

    public void SetSfxVolume(float sfxVol)
    {
        masterMixer.SetFloat(SFX_VOLUME_KEY, sfxVol);
        PlayerPrefs.SetFloat(SFX_VOLUME_KEY, sfxVol);
    }

    public void SetMusicVolume(float musixVol)
    {
        masterMixer.SetFloat(MUSIC_VOLUME_KEY, musixVol);
        PlayerPrefs.SetFloat(MUSIC_VOLUME_KEY, musixVol);
    }

}

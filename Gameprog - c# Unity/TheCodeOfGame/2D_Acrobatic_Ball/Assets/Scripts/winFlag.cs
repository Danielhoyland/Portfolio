using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class winFlag : MonoBehaviour
{
    private void OnCollisionEnter2D(Collision2D collision)
    {
        if (collision.gameObject.CompareTag("Player")) {
            PlayWinSound();
            LevelManager.instance.Win();
        }
    }

    private void PlayWinSound()
    {
        AudioSource audioSource = GetComponent<AudioSource>();
        audioSource.Play();
    }
}

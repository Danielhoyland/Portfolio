using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Tilemaps;

public class SlowBox : MonoBehaviour
{
    public Transform playerTransform;
    public Rigidbody2D playerRigid;
    Tilemap tilemap;

    public float speedReductionPercentage = 0.9f; 

    [SerializeField] private GameObject boxParticlesPrefab;
    [SerializeField] private GameObject boxBreakingSoundPrefab;

    private void Start()
    {
        tilemap = FindObjectOfType<Tilemap>();
    }

    private void Update()
    {
        if (playerTransform != null)
        {
            Vector3 playerPosition = playerTransform.position;

            Vector3Int playerCell = tilemap.WorldToCell(playerPosition);

            TileBase tile = tilemap.GetTile(playerCell);

            if (tile != null && tile.name == "WoodBox1")
            {
                playerRigid.velocity = playerRigid.velocity * (1.0f - speedReductionPercentage);

                tilemap.SetTile(playerCell, null);

                InitParticles();
                PlayBreakingSound();
            }
        }
    }

    private void InitParticles()
    {
        GameObject particles = Instantiate(
            boxParticlesPrefab,
            playerTransform.position - new Vector3(0f, 0.5f, 0f),
            boxParticlesPrefab.transform.rotation);

        ParticleSystem particleSystem = particles.GetComponent<ParticleSystem>();
        particleSystem.Play();
    }

    private void PlayBreakingSound()
    {
        GameObject boxBreakingAudioSourceObject = Instantiate(
            boxBreakingSoundPrefab,
            playerTransform.position,
            boxBreakingSoundPrefab.transform.rotation);

        AudioSource audioSource = boxBreakingAudioSourceObject.GetComponent<AudioSource>();
        audioSource.Play();

        Destroy(boxBreakingAudioSourceObject, audioSource.clip.length);
    }
}

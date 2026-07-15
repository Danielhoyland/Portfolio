using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Tilemaps;

public class Spike : MonoBehaviour
{
    public Transform playerTransform;
    Tilemap tilemap;
    bool state;
    private void Start()
    {
        tilemap = FindObjectOfType<Tilemap>();
        state = false;
    }

    private void Update()
    {
        if (playerTransform != null)
        {
            Vector3 playerPosition = playerTransform.position;

            Vector3Int playerCell = tilemap.WorldToCell(playerPosition);

            TileBase tile = tilemap.GetTile(playerCell);

            if (tile != null && tile.name == "Needle1" && !state)
            {
                LevelManager.instance.GameOver();   
                GameObject.FindGameObjectWithTag("Player").GetComponentInChildren<SpriteRenderer>().enabled = false;
                GameObject.FindGameObjectWithTag("Player").GetComponent<Rigidbody2D>().simulated = false;
                state = true;
            }
        }
    }
}

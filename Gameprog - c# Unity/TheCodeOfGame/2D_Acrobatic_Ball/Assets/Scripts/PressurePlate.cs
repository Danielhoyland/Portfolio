using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PressurePlate : MonoBehaviour
{
    public Vector3Int targetCell;
    public Door door;

    private void OnTriggerEnter2D(Collider2D collision)
    {
        if (collision.gameObject.CompareTag("Ball"))
        {   
            door.startingCell = targetCell;
            door.isOpen = true;
            
        }
    }
}

using UnityEngine;
using UnityEngine.Tilemaps;

public class Door : MonoBehaviour
{
    public Tilemap tilemap;
    public Vector3Int startingCell;
    public Vector3Int offset;
    public bool isOpen = false; 
    public bool hasOpen = false; 
    public float openSpeed = 2.0f;

    public delegate void OnDoorOpen();
    public static event OnDoorOpen DoorOpen;

    Vector3Int[] GetCellPositions(Vector3Int startingCell)
    {
        // If the starting postion is the upper left of 4 squares
        Vector3Int[] cellPositions = new Vector3Int[4];
        cellPositions[0] = startingCell;
        cellPositions[1] = startingCell + new Vector3Int(1, 0, 0);
        cellPositions[2] = startingCell + new Vector3Int(0, -1, 0);
        cellPositions[3] = startingCell + new Vector3Int(1, -1, 0);
        return cellPositions;
    }

    void Update()
    {
        if (!hasOpen && BallInPlace())
        {
            DoorOpen?.Invoke();
            OpenDoor();
        }
    }

    bool BallInPlace()
    {
        if (isOpen)
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    void OpenDoor()
    {
        Vector3Int[] cellPositions = GetCellPositions(startingCell);

        foreach (Vector3Int cellPosition in cellPositions)
        {   
            TileBase tile = tilemap.GetTile(cellPosition); 
            tilemap.SetTile(cellPosition, null); 
            tilemap.SetTile(cellPosition + offset, tile); 
        }

        hasOpen = true; 
    }
}

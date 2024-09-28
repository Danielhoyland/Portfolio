using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FireBall : MonoBehaviour
{
    public float speed = 25.0f;
    public float destroyDistance = 20.0f; 

    public Vector2 direction;

    private Transform playerTransform;

    void Start()
    {
        playerTransform = GameObject.FindGameObjectWithTag("Player")?.transform;
        if (playerTransform == null)
        {
            Destroy(gameObject);
        }
        else
        {
            direction = (playerTransform.position - transform.position).normalized;
        }
    }

    void Update()
    {
        transform.Translate(direction * speed * Time.deltaTime);

        float distanceToPlayer = Vector3.Distance(transform.position, playerTransform.position);

        if (distanceToPlayer > destroyDistance)
        {
            Destroy(gameObject);
        }
    }

    void OnCollisionEnter2D(Collision2D collision)
    {
        if (collision.gameObject.CompareTag("Player"))
        {
            LevelManager.instance.GameOver();
            playerTransform.GetComponentInChildren<SpriteRenderer>().enabled = false;
            playerTransform.GetComponent<Rigidbody2D>().simulated = false;
            Destroy(gameObject);
        }
        else
        {
            Destroy(gameObject);
        }
    }
}

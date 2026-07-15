using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class WizardEnemy : MonoBehaviour
{
    private Animator animator;
    public GameObject fireBallPrefab;
    [SerializeField] float shootCooldown = 4.0f;
    public bool death = false;

    private Transform playerTransform;
    private float lastShootTime = 0;

    void Start()
    {
        animator = GetComponent<Animator>();
        playerTransform = GameObject.FindGameObjectWithTag("Player").transform;
        if (playerTransform == null)
        {
            Debug.LogError("Player not found! Make sure the player has the 'Player' tag.");
        }
    }

    void Update()
    {
        if (!death)
        {
            if (Time.time - lastShootTime > shootCooldown)
            {
                if (CanSeePlayer())
                {
                    animator.SetBool("IsShooting", true);
                    lastShootTime = Time.time;
                }
            }
            else if (lastShootTime < Time.time - 0.217f)
            {
                animator.SetBool("IsShooting", false);
            }
        }
        else if (death)
        {
            animator.SetBool("IsDead", death);
        }
        
    }

    void ShootFireBall()
    {
        if (fireBallPrefab != null && playerTransform != null)
        {
            // Calculate the direction to the player
            Vector2 shootDirection = (playerTransform.position - transform.position).normalized;

            // Flip the enemy wizard's sprite to face the player
            if (shootDirection.x < 0)
            {
                // Calculate the local scale based on the original scale
                Vector3 localScale = transform.localScale;
                localScale.x = Mathf.Abs(localScale.x) * -1;
                transform.localScale = localScale;
            }
            else
            {
                // Calculate the local scale based on the original scale
                Vector3 localScale = transform.localScale;
                localScale.x = Mathf.Abs(localScale.x);
                transform.localScale = localScale;
            }

            // Set the spawn position for the fireball
            Vector3 spawnPosition = transform.position + new Vector3(shootDirection.x * 1.0f, 0, 0);

            // Instantiate and set the direction of the fireball
            GameObject fireBall = Instantiate(fireBallPrefab, spawnPosition, Quaternion.identity);
            fireBall.GetComponent<FireBall>().direction = shootDirection;
        }
    }

    void Death()
    {
        Destroy(gameObject);
    }
    bool CanSeePlayer()
    {
        Vector2 directionToPlayer = (playerTransform.position - transform.position).normalized;
        LayerMask layerMask = LayerMask.GetMask("Platform");

        RaycastHit2D hit = Physics2D.Raycast(transform.position, directionToPlayer, Vector2.Distance(transform.position, playerTransform.position), layerMask);

        if (hit.collider != null)
        {
            return false;
        }
        return true;
    }
}

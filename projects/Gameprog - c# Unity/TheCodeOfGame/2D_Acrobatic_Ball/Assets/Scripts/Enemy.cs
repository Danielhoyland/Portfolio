using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Enemy : MonoBehaviour
{
    private Animator animator;
    private Transform playerTransform;
    private Transform enemyTransform;
    [SerializeField] float moveSpeed = 8.0f;
    public bool death = false;


    Vector2 slopeTangent;

    CapsuleCollider2D capsuleCollider;
    Rigidbody2D rb2d;
    Vector2 standingCapsuleColliderSize;
    Vector2 standingCapsuleColliderOffset;

    [SerializeField] LayerMask platformLayer;

    void Start()
    {
        capsuleCollider = GetComponent<CapsuleCollider2D>();
        rb2d = GetComponent<Rigidbody2D>();
        animator = GetComponent<Animator>();
        standingCapsuleColliderSize = capsuleCollider.size;
        standingCapsuleColliderOffset = capsuleCollider.offset;
        playerTransform = GameObject.FindGameObjectWithTag("Player").transform;
        enemyTransform = GetComponent<Transform>();
        if (playerTransform == null)
        {
            Debug.LogError("Player not found! Make sure the player has the 'Player' tag.");
        }
    }

    void Update()
    {
        if (!death)
        {
            if (IsPlayerInCameraView())
            {
                Vector2 moveDirection = new Vector2(1, 0); 

                bool isRunning = moveDirection.magnitude > 0.1f;
                animator.SetBool("IsRunning", isRunning);

                if (playerTransform != null)
                {
                    
                    Vector2 direction = (playerTransform.position - transform.position).normalized;

                    if (IsOnSlope()) rb2d.gravityScale = 0f;
                    else rb2d.gravityScale = 3.0f;

                    if (isRunning)
                    {
                        if (direction.x < 0) 
                        {
                            Vector3 localScale = transform.localScale;
                            localScale.x = Mathf.Abs(localScale.x) * -1;
                            transform.localScale = localScale;
                        }
                        else 
                        {
                            Vector3 localScale = transform.localScale;
                            localScale.x = Mathf.Abs(localScale.x);
                            transform.localScale = localScale;
                        }
                    }

                    transform.Translate(direction * moveSpeed * Time.deltaTime);
                }
            }
        }
        else if (death)
        {
            animator.SetBool("IsDead", death);
        }
    }

    private void OnCollisionEnter2D(Collision2D collision)
    {
        if (collision.gameObject.CompareTag("Player"))
        {
            LevelManager.instance.GameOver();   
            GameObject.FindGameObjectWithTag("Player").GetComponentInChildren<SpriteRenderer>().enabled = false;
            GameObject.FindGameObjectWithTag("Player").GetComponent<Rigidbody2D>().simulated = false;
        }
    }

    void Death()
    {
        Destroy(gameObject);
    }

    bool IsPlayerInCameraView()
    {
        Camera mainCamera = Camera.main;

        if (mainCamera == null)
            return false;

        Vector3 screenPos = mainCamera.WorldToViewportPoint(enemyTransform.position);
        return screenPos.x >= 0 && screenPos.x <= 1 && screenPos.y >= 0 && screenPos.y <= 1;
    }
    bool IsOnSlope()
    {
        Vector2 rayCastOrigin = new Vector2(enemyTransform.position.x, enemyTransform.position.y + capsuleCollider.offset.y - capsuleCollider.size.y + 0.1f);
        RaycastHit2D hit = Physics2D.Raycast(rayCastOrigin, Vector2.down, 1.0f, platformLayer);

        return hit;
    }


}

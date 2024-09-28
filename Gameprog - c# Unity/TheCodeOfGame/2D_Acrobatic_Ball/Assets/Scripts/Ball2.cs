using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Ball2 : MonoBehaviour
{
    [HideInInspector] public PlayerStateMachine player = null;

    private bool returningToPlayer = false;
    [SerializeField] private float returningToPlayerInitialSpeed = 10;
    [SerializeField] private float returningToPlayerSpeedAccel = 25;
    private float returningToPlayerCurrSpeed = 0;
    private Vector3 returningToPlayer_startPos = Vector3.zero;

    public delegate void OnBallReturnedToPlayer();
    public static event OnBallReturnedToPlayer BallReturnedToPlayer;

    public void BeginReturnToPlayer()
    {
        if (returningToPlayer)
            return;

        returningToPlayer = true;
        returningToPlayer_startPos = transform.position;
        returningToPlayerCurrSpeed = returningToPlayerInitialSpeed;
        var rb = GetComponent<Rigidbody2D>();
        if (rb != null)
        {
            Destroy(rb);
        }
        var coll = GetComponent<Collider2D>();
        if (coll != null)
        {
            Destroy(coll);
        }
    }

    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        var deltaTime = Time.deltaTime;

        if (returningToPlayer)
        {
            // Vector towards player
            var toPlayer = player.transform.position - transform.position;

            var playerPos2 = new Vector2(player.transform.position.x, player.transform.position.y);
            var ballReturnStart2 = new Vector2(returningToPlayer_startPos.x, returningToPlayer_startPos.y);
            var ballPos2 = new Vector2(transform.position.x, transform.position.y);

            // We check if the ball is closer than 1 meter to the player,
            // or if the ball has completely passed the player relative to the initial summoning position
            var playerToBallStart = (ballReturnStart2 - playerPos2).normalized;
            var playerToBallNow = (ballPos2 - playerPos2).normalized;
            var dot = Vector2.Dot(playerToBallNow, playerToBallStart);
            bool hasPassedPlayer = dot <= 0;
            if (hasPassedPlayer)
            {
                // Destroy ourselves and trigger event on Player.
                player.OnBallReturn();
                Destroy(gameObject);
                BallReturnedToPlayer?.Invoke();
                player.ballCatchSound.Play();
                return;
            }

            transform.position += toPlayer.normalized * returningToPlayerCurrSpeed * deltaTime;

            returningToPlayerCurrSpeed += returningToPlayerSpeedAccel * deltaTime;
        }
    }

    void FixedUpdate()
    {



    }
}

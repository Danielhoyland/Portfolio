using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using UnityEngine;

public class PhysicsPlatform : MonoBehaviour
{
    
    [SerializeField] private float maxRotation = 2.0f;
    [SerializeField] private float selfCorrectRotationMultiplier = 20.0f;
    public float GetSelfCorrectRotationMultiplier() { return selfCorrectRotationMultiplier; }
    [SerializeField] private float _selfForceStrengthMultiplier = 5.0f;
    public float GetSelfForceStrengthMultiplier() { return _selfForceStrengthMultiplier; }
    public float GetMaxRotation() { return maxRotation; }
    [SerializeField] private bool useEndPos = false;
    public bool GetUseEndPos() { return useEndPos; }
    [SerializeField] private Vector2 endPos = new Vector2();
    public Vector2 GetEndPos() { return endPos; }
    public void Internal_SetEndPos(Vector2 newTarget) { endPos = newTarget; }
    [SerializeField] private float cyclesPerMinute = 10;
    [SerializeField] private bool waitForPlayer = false;

    private bool shouldStart = false;




    private double startTime = 0;
    private Vector2 startPos = new Vector2();
    private float startRotation = 0;


    public Vector2 CalcTargetPoint()
    {
        if (GetUseEndPos())
        {
            var cos = -Mathf.Cos((float)(Time.timeAsDouble - startTime) * 2 * Mathf.PI / 60 * cyclesPerMinute);
            cos = (cos + 1) / 2;
            // Then map it to [0, 1] range
            var targetPos = Vector2.Lerp(startPos, endPos, cos);
            return targetPos;
        }
        else
        {
            return startPos;
        }

    }

    //check if player has touched the platform
    private void OnCollisionEnter2D(Collision2D collision)
    {
        if (!shouldStart && collision.gameObject.tag == "Player")
        {
            shouldStart = true;
            startTime = Time.timeAsDouble;
        }
    }

    // Start is called before the first frame update
    void Start()
    {
        startPos = transform.position;
        startTime = Time.timeAsDouble;
        startRotation = transform.rotation.eulerAngles.z;
        if (!waitForPlayer ) { shouldStart = true; }
    }

    // Update is called once per frame
    void Update()
    {
        
    }

    void FixedUpdate()
    {
        // Apply force towards our current target position
        if(shouldStart)
        {
            var rb = GetComponent<Rigidbody2D>();
            if (rb == null)
            {
                Debug.LogError("Found no rigidbody on moving platform");
            }

            var currPos = new Vector2(transform.position.x, transform.position.y);

            var targetPos = CalcTargetPoint();
            var toTargetPos = targetPos - currPos;

            if (toTargetPos.magnitude > 0.01f)
            {
                rb.AddForce(toTargetPos * rb.mass * GetSelfForceStrengthMultiplier());
            }

            rb.AddTorque(-rb.rotation * GetSelfCorrectRotationMultiplier() * rb.mass);

            if (rb.rotation < -GetMaxRotation())
            {
                rb.SetRotation(-GetMaxRotation());
            }
            else if (rb.rotation > GetMaxRotation())
            {
                rb.SetRotation(GetMaxRotation());
            }
        }
    }
}

using System.Drawing;
using Unity.Burst;
using UnityEngine;
using static UnityEngine.GridBrushBase;

/**
 * Class that handles camera shake logic
 * 
 * Inspiration: https://www.youtube.com/watch?v=fn3hIPLbSn8&t=282s
 */
public abstract class CameraShake
{
    // Time
    private float startTime = -10f;
    protected float duration;

    // Position
    public Vector3 currentMovement { get; protected set; } = Vector3.zero;

    // Rotation
    public float currentRotation { get; protected set; } = 0f;


    /**
     * Constructor
     */
    public CameraShake(float duration)
    {
        this.duration = duration;
    }


    /**
     * Starts camera shake
     */
    public void Start()
    {
        startTime = Time.time;
    }

    /**
     * Updates camera shake
     */
    public abstract void Update();


    /**
     * Returns the time left of the camera shake
     */
    public float TimeLeft() => startTime + duration - Time.time;

    /**
     * Returns the time used so far
     */
    public float TimeUsed() => Time.time - startTime;

    /**
     * Returns tha protion of time left as a float between 1 and 0
     */
    public float PortionOfTimeLeft() => TimeLeft() / duration;


    #region MathUtils

    /**
     * Decreases the startvalue down to zero as a square root equation
     * Faster than QuadDownToZero() in the begining, but slower towards the end
     */
    protected float SqrtDownToZero(float startValue, float x)
    {
        return startValue - Mathf.Sqrt(startValue - startValue * x) * Mathf.Sqrt(startValue);
    }

    /**
     * Decreases the startvalue down to zero as a quadratic equation
     * Slower than SqrtDownToZero() in the begining, but faster towards the end
     */
    protected float QuadDownToZero(float startValue, float x)
    {
        return startValue * x * x;
    }

    /// <summary>
    /// Changes a value over time as a quadratic equation.<br/>
    /// For slow start and fast end: Increase `x` over time.<br/>
    /// For fast start and slow end: Set `x` to `duration`, decrese `x` over time, and switch `maxY` and `minY`.
    /// </summary> 
    /// <param name="duration">The timespan of the value change</param>
    /// <param name="x">Relative current time</param>
    /// <param name="maxY"></param>
    /// <param name="minY"></param>
    protected float Quad(float duration, float x, float maxY, float minY = 0f)
    {
        float a = (maxY - minY) / (duration * duration);
        float c = minY;
        return a*x*x + c;
    }

    #endregion
}
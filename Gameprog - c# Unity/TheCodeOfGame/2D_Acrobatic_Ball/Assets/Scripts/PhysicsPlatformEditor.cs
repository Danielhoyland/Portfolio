using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;

[CustomEditor(typeof(PhysicsPlatform))]
public class PhysicsPlatformEditor : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector(); // Draw the default inspector

        var component = (PhysicsPlatform)target;

        // Add a button to reset the target position
        if (GUILayout.Button("Reset Target Position"))
        {
            Undo.RecordObject(component, "Move Target Position");
            component.Internal_SetEndPos(component.transform.position + Vector3.one);
            EditorUtility.SetDirty(component);
        }
    }

    void OnSceneGUI()
    {
        var platform = (PhysicsPlatform)target;

        var currTargetPos = platform.GetEndPos();

        if (platform.GetUseEndPos())
        {
            // Draw a handle to move the point
            EditorGUI.BeginChangeCheck();
            var newTargetPos = Handles.PositionHandle(currTargetPos, Quaternion.identity);
            // Draw a small sphere as a gizmo
            Handles.SphereHandleCap(
                0,
                newTargetPos,
                Quaternion.identity,
                0.5f,
                EventType.Repaint
            );

            // Draw debug text
            GUIStyle textStyle = new GUIStyle();
            textStyle.normal.textColor = Color.grey;
            textStyle.fontSize = 16;
            var labelPosition = newTargetPos + Vector3.down * 0.2f + Vector3.left * 2.0f; // Adjust the offset as needed
            Handles.Label(labelPosition, "Target pos", textStyle);


            Handles.color = Color.cyan;
            Handles.DrawLine(platform.transform.position, newTargetPos);


            if (EditorGUI.EndChangeCheck())
            {
                Undo.RecordObject(platform, "Move Target Position");
                platform.Internal_SetEndPos(newTargetPos);
                EditorUtility.SetDirty(platform);
            }
        }
    }
}
#endif
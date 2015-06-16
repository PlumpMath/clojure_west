using UnityEngine;
using System.Collections;

public class RayCastHelper {
    public static RaycastHit[] raycast(Ray ray) {
    	RaycastHit[] hits = new RaycastHit[0];
    	RaycastHit hit;
        if (Physics.Raycast(ray, out hit)) {
        	hits = new RaycastHit[1];
        	hits[0] = hit;
        }

        return hits;
    }
}
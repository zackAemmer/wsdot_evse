-- What is up with plug connector codes? Lots of built evse not included

-- Select infrastructure along infeasible route closest to the midpoint
-- Limit to cities/gas stations that are within 5 mile buffer of infeasible segment
SELECT *
INTO combo_candidates_wsdot
FROM
	(SELECT segments.gid,
			segments.trip_count,
			candidates.id,
			candidates.type,
			candidates.geom,
			segments.length,
			ABS(candidates.ratio - .5) AS dist_to_center,
			ROW_NUMBER() OVER (PARTITION BY segments.gid ORDER BY ABS(candidates.ratio - .5) ASC) AS rank
	FROM trip_infeasibility_combo_wsdot AS segments
	CROSS JOIN LATERAL
	(SELECT id,
			geom,
			'CITY' AS "type",
			ST_LineLocatePoint(segments.geom, geom) AS ratio
	FROM city_limits_centroids
	UNION ALL
	SELECT id,
			geom,
			'GAS' AS "type",
			ST_LineLocatePoint(segments.geom, geom) AS ratio
	FROM all_gas_stations
	WHERE ST_DWithin(segments.geom, geom, .112)) AS candidates) AS ranked_locations
WHERE rank = 1
ORDER BY trip_count DESC


-- Cluster candidate sites based on DBSCAN algorithm
SELECT *,
		ST_ClusterDBSCAN(geom, eps:=.2, minpoints:=1) OVER () AS cluster_id
INTO candidate_clusters_test
FROM combo_candidates_wsdot_2

-- Select the best candidate site from each spatial cluster
SELECT cid_count,
		cid,
		cluster_id,
		geom
INTO combo_candidates_wsdot_3
FROM
(SELECT cid_counts.cid_count,
 		cid_counts.cid,
 		candidate_clusters.cluster_id,
 		geom,
		ROW_NUMBER() OVER (PARTITION BY cluster_id ORDER BY cid_count DESC) AS rank
FROM candidate_clusters
JOIN
(SELECT COUNT(*) AS cid_count, cid
FROM candidate_clusters
GROUP BY cid) AS cid_counts
ON candidate_clusters.cid = cid_counts.cid) AS ranked_cids
WHERE rank = 1
ORDER BY cid DESC

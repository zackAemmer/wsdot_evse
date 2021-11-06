-- What is up with plug connector codes? Lots of built evse not included

-- Cluster candidate sites based on DBSCAN algorithm
-- Currently using QGIS DBSCAN with .112 (5mi) max spacings and treat edge cases as noise
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

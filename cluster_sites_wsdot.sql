-- Cluster candidate sites based on DBSCAN algorithm
SELECT *,
		ST_ClusterDBSCAN(geom, eps:=.24, minpoints:=1) OVER () AS cluster_id
INTO combo_candidates_clustered_wsdot
FROM combo_candidates_wsdot

-- Select the best candidate site from each spatial cluster
SELECT cid_count,
		cid,
		cluster_id,
		geom
INTO combo_candidates_final_wsdot
FROM
(SELECT cid_counts.cid_count,
 		cid_counts.cid,
 		combo_candidates_clustered_wsdot.cluster_id,
 		geom,
		ROW_NUMBER() OVER (PARTITION BY cluster_id ORDER BY cid_count DESC) AS rank
FROM combo_candidates_clustered_wsdot
JOIN
(SELECT COUNT(*) AS cid_count, cid
FROM combo_candidates_clustered_wsdot
GROUP BY cid) AS cid_counts
ON combo_candidates_clustered_wsdot.cid = cid_counts.cid) AS ranked_cids
WHERE rank = 1
ORDER BY cid DESC

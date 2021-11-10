-- Cluster candidate sites based on DBSCAN algorithm w/10mi radius
SELECT *,
		ST_ClusterDBSCAN(geom, eps:=.24, minpoints:=1) OVER () AS cluster_id
INTO combo_candidates_clustered_wsdot
FROM combo_candidates_wsdot;

-- Select the best candidate site from each spatial cluster
SELECT *
INTO combo_candidates_final_wsdot
FROM
(SELECT cid_counts.cid,
 		cid_counts.cid_count,
 		combo_candidates_clustered_wsdot.cluster_id,
 		combo_candidates_clustered_wsdot.trip_count,
 		combo_candidates_clustered_wsdot.bev_count,
 		combo_candidates_clustered_wsdot.dist_bin,
 		combo_candidates_clustered_wsdot.dist_to_desired
 		geom,
		ROW_NUMBER() OVER (PARTITION BY cluster_id ORDER BY cid_count DESC) AS rank
FROM combo_candidates_clustered_wsdot
JOIN
(SELECT COUNT(*) AS cid_count, cid
FROM combo_candidates_clustered_wsdot
GROUP BY cid) AS cid_counts
ON combo_candidates_clustered_wsdot.cid = cid_counts.cid) AS ranked_cids
WHERE ranked_cids.rank = 1
ORDER BY cid DESC;
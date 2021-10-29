-- Need to add functionality for over 100 miles, or run multiple times (100; 2, 150; 3, 200; 4, 250; 5...)
-- Infeasible segments can overlap, and tend to have similar trip_count
-- How to place so that we don't put multiple evse on overlapping segments
-- What is up with plug connector codes? Lots of built evse not included

-- Get cities/gas stations that are within 5 mile buffer of infeasible segment
-- Limit to candidate site that is closest to the halfway point
-- Select into chademo_candidates_wsdot
-- ~1minute
SELECT *
INTO chademo_candidates_wsdot
FROM
	(SELECT segments.gid,
			segments.trip_count,
			candidates.id,
			candidates.type,
			candidates.geom,
			segments.length,
			ABS(candidates.ratio - .5) AS dist_to_center,
			ROW_NUMBER() OVER (PARTITION BY segments.gid ORDER BY ABS(candidates.ratio - .5) ASC) AS rank
	FROM trip_infeasibility_chademo_wsdot AS segments
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
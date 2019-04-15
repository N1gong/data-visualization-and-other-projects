--mean and variance
CREATE VIEW temp1 AS
SELECT regionidcounty, regionidzip AS zip, round(variance(taxvaluedollarcnt), 2) AS Variance, round(avg(taxvaluedollarcnt), 2) AS Mean
FROM (SELECT regionidcounty, regionidzip, taxvaluedollarcnt
FROM housing2016
WHERE regionidcounty = 3101 OR regionidcounty = 1286) AS tempdata
WHERE regionidzip IS NOT NULL
GROUP BY regionidcounty, regionidzip
ORDER BY regionidcounty, regionidzip;

--Compute histogram
CREATE VIEW hist AS
SELECT regionidcounty, regionidzip, count(regionidzip)
FROM (SELECT regionidcounty, regionidzip, taxvaluedollarcnt
FROM housing2016
WHERE (regionidcounty = 3101 OR regionidcounty = 1286) AND taxvaluedollarcnt IS NOT NULL) AS tempdata
GROUP BY regionidzip, regionidcounty
ORDER BY regionidzip, regionidcounty;


--create two groups and find indicator variable
CREATE VIEW p_high AS
SELECT 0 AS first, avg(bathroomcnt) AS avg_bathcnt, avg(calculatedbathnbr) AS avg_calbathcnt, avg(fullbathcnt) AS avg_fullbath,
       avg(bedroomcnt) AS avg_bedcnt, avg(calculatedfinishedsquarefeet) AS avg_sqft, avg(lotsizesquarefeet) AS lotsize,
       avg(roomcnt) AS avg_rmcnt, avg(unitcnt) AS avg_unit, avg(yearbuilt) AS avg_year, avg(numberofstories) AS avg_stories,
       avg(structuretaxvaluedollarcnt) AS avg_structurevalue, avg(assessmentyear) AS avg_assessyear, avg(landtaxvaluedollarcnt) AS avg_landvalue,
       avg(taxamount) AS avg_tax
FROM housing2016
WHERE taxvaluedollarcnt > 4121437.77 AND (regionidcounty = 1286 OR regionidcounty = 3101)
AND taxvaluedollarcnt is not NULL;

CREATE VIEW p_low AS
SELECT 0 AS first, avg(bathroomcnt) AS avg_bathcnt, avg(calculatedbathnbr) AS avg_calbathcnt, avg(fullbathcnt) AS avg_fullbath,
       avg(bedroomcnt) AS avg_bedcnt, avg(calculatedfinishedsquarefeet) AS avg_sqft, avg(lotsizesquarefeet) AS lotsize,
       avg(roomcnt) AS avg_rmcnt, avg(unitcnt) AS avg_unit, avg(yearbuilt) AS avg_year, avg(numberofstories) AS avg_stories,
       avg(structuretaxvaluedollarcnt) AS avg_structurevalue, avg(assessmentyear) AS avg_assessyear,
       avg(landtaxvaluedollarcnt) AS avg_landvalue, avg(taxamount) AS avg_tax
FROM housing2016
WHERE taxvaluedollarcnt < 4121437.77 AND (regionidcounty = 1286 OR regionidcounty = 3101)
AND taxvaluedollarcnt is not NULL;

CREATE VIEW diff AS
select (p_high.avg_bathcnt-p_low.avg_bathcnt)/p_low.avg_bathcnt AS diff_bathcnt, (p_high.avg_calbathcnt-p_low.avg_calbathcnt)/p_low.avg_calbathcnt AS diff_calbathcnt,
       (p_high.avg_fullbath-p_low.avg_fullbath)/p_low.avg_fullbath AS diff_fullbath,
       (p_high.avg_bedcnt-p_low.avg_bedcnt)/p_low.avg_bedcnt AS diff_bedcnt,
       (p_high.avg_sqft-p_low.avg_sqft)/p_low.avg_sqft AS diff_sqft,
       (p_high.lotsize-p_low.lotsize)/p_low.lotsize AS diff_lotsize,
       (p_high.avg_rmcnt-p_low.avg_rmcnt)/p_low.avg_rmcnt AS diff_rmcnt,
       (p_high.avg_unit-p_low.avg_unit)/p_low.avg_unit AS diff_unit,
       (p_high.avg_year-p_low.avg_year)/p_low.avg_year AS diff_year,
       (p_high.avg_stories-p_low.avg_stories)/p_low.avg_stories AS diff_stories,
       (p_high.avg_structurevalue-p_low.avg_structurevalue)/p_low.avg_structurevalue AS diff_structurevalue,
       (p_high.avg_assessyear-p_low.avg_assessyear)/p_low.avg_assessyear AS diff_assessyear,
       (p_high.avg_landvalue-p_low.avg_landvalue)/p_low.avg_landvalue AS diff_landvalue,
       (p_high.avg_tax-p_low.avg_tax)/p_low.avg_tax AS diff_tax
from p_high join p_low on p_high.first = p_low.first


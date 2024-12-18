-- TEST MVU FOR PATINDEX, CHARINDEX, REPLACE


CREATE PROCEDURE babel_5144_p (@src sys.NVARCHAR(100), @from sys.NVARCHAR(100), @to sys.NVARCHAR(100)) AS
SELECT 'running replace'
SELECT replace(@src, @from, @to);
SELECT 'running charindex'
SELECT charindex(@from, @src);
SELECT 'running patindex'
SELECT patindex('%de%', @src);
GO

CREATE FUNCTION babel_5144_f1(@src sys.NVARCHAR(100), @from sys.NVARCHAR(100), @to sys.NVARCHAR(100)) RETURNS NVARCHAR(100)
AS
BEGIN
    RETURN replace(@src, @from, @to);
END
GO

CREATE FUNCTION babel_5144_f2(@src sys.NVARCHAR(100), @ch sys.NVARCHAR(100)) RETURNS NVARCHAR(100)
AS
BEGIN
    RETURN charindex(@ch, @src);
END
GO

CREATE FUNCTION babel_5144_f3(@src sys.NVARCHAR(100)) RETURNS NVARCHAR(100)
AS
BEGIN
    RETURN patindex('%de%', @src);
END
GO

CREATE TABLE babel_5144_t1 (src NVARCHAR(100),
                            substr1 NVARCHAR(100),
                            substr2 NVARCHAR(100),
                            [replaced] AS replace(src, substr1, substr2),
                            [charIndex] as charindex(substr1, src),
                            [patindex] AS patindex('%de%', src));
GO

CREATE TABLE babel_5144_t2 (src NVARCHAR(100),
                            substr1 NVARCHAR(100),
                            substr2 NVARCHAR(100),
                            CHECK (replace(src, substr1, substr2) != src),
                            CHECK (charindex(substr1, src) >= 4),
                            CHECK (patindex('%de%', src) > 0));
GO

CREATE TABLE babel_5144_t3 (src NVARCHAR(100),
                            substr1 NVARCHAR(100),
                            substr2 NVARCHAR(100),
                            [replaced] NVARCHAR(100) DEFAULT replace(CAST('abcḍèĎÈdedEDEabcd' AS NVARCHAR), CAST('de' AS NVARCHAR), CAST('##' AS NVARCHAR)),
                            [charIndex] INT DEFAULT charindex(CAST('de' AS NVARCHAR) ,CAST('abcḍèĎÈdedEDEabcd' AS NVARCHAR)),
                            [patindex] INT DEFAULT patindex(CAST('%de%' AS NVARCHAR) ,CAST('abcḍèĎÈdedEDEabcd' AS NVARCHAR)));
GO

/* INDEX on computed columns */
CREATE INDEX babel_5144_idx1 ON babel_5144_t1 ([replaced], [charIndex], [patindex])
GO
CREATE INDEX babel_5144_idx3 ON babel_5144_t3 ([replaced], [charIndex], [patindex])
GO

CREATE TRIGGER babel_5144_trigger
ON babel_5144_t1
AFTER INSERT
AS
    SELECT '========== trigger start =========='
    SELECT replace(src, substr1, substr2) AS [replaced],
            charindex(substr1, src) AS [charIndex],
            patindex('%de%', src) AS [patindex]
    FROM babel_5144_t1;
    SELECT '========== trigger end   =========='
GO

CREATE VIEW babel_5144_v1 AS
    SELECT [a].[replaced] AS r1, [b].[replaced] AS r2, [c].[replaced] AS r3,
           [a].[charIndex] AS c1, [b].[charIndex] AS c2, [c].[charIndex] AS c3,
           [a].[patindex] AS p1, [b].[patindex] AS p2, [c].[patindex] AS p3
    FROM 
        (SELECT replace(src, substr1, substr2) AS [replaced], charindex(substr1, src) AS [charIndex], patindex('%de%', src) AS [patindex] FROM babel_5144_t2) [a]
        JOIN babel_5144_t1 [b] ON ([b].[replaced] = [a].[replaced])
        JOIN babel_5144_t3 [c] ON ([c].[replaced] = [a].[replaced]);
GO

INSERT INTO babel_5144_t1 VALUES ('abcḍèĎÈdedEDEabcd', 'de', '##')
INSERT INTO babel_5144_t2 VALUES ('abcḍèĎÈdedEDEabcd', 'de', '##')
INSERT INTO babel_5144_t3(src, substr1, substr2) VALUES ('abcḍèĎÈdedEDEabcd', 'de', '##')
GO

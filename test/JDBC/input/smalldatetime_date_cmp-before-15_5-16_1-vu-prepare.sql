CREATE TABLE smalldate_date_cmp_t1 (
    smalldatetime_col SMALLDATETIME
)
GO

INSERT INTO smalldate_date_cmp_t1 (smalldatetime_col) SELECT cast('2023-06-13 14:30:00' as smalldatetime) from generate_series(1, 10)
GO

INSERT INTO smalldate_date_cmp_t1 (smalldatetime_col) SELECT cast('2023-06-15 14:30:00' as smalldatetime) from generate_series(1, 100000)
GO

INSERT INTO smalldate_date_cmp_t1 (smalldatetime_col) SELECT cast('2023-06-17 14:30:00' as smalldatetime) from generate_series(1, 10)
GO

CREATE NONCLUSTERED INDEX smalldate_date_cmp_ind1 ON smalldate_date_cmp_t1
(
    smalldatetime_col ASC
)
GO

-- seq scan
CREATE PROCEDURE test_smalldatetime_date_p1 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <> CAST('2023-06-13' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p2 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <> CAST('2023-06-15' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p3 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <> CAST('2023-06-17' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p4 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-13' AS DATE) <> smalldatetime_col;
GO

CREATE PROCEDURE test_smalldatetime_date_p5 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-15' AS DATE) <> smalldatetime_col;
GO

CREATE PROCEDURE test_smalldatetime_date_p6 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-17' AS DATE) <> smalldatetime_col;
GO

-- index scan
CREATE PROCEDURE test_smalldatetime_date_p7 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col < CAST('2023-06-14' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p8 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <= CAST('2023-06-14' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p9 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <= CAST('2023-06-18' AS DATE) AND smalldatetime_col >= CAST('2023-06-16' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p10 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col = CAST('2023-06-17' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p11 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col > CAST('2023-06-16' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p12 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col >= CAST('2023-06-16' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p13 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col BETWEEN CAST('2023-06-16' AS DATE) AND CAST('2023-06-18' AS DATE);
GO

CREATE PROCEDURE test_smalldatetime_date_p14 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col IS NULL
GO

CREATE PROCEDURE test_smalldatetime_date_p15 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-14' AS DATE) > smalldatetime_col;
GO

CREATE PROCEDURE test_smalldatetime_date_p16 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-14' AS DATE) >= smalldatetime_col;
GO

CREATE PROCEDURE test_smalldatetime_date_p17 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-16' AS DATE) <= smalldatetime_col AND CAST('2023-06-18' AS DATE) >= smalldatetime_col;
GO

CREATE PROCEDURE test_smalldatetime_date_p18 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-17' AS DATE) = smalldatetime_col;
GO

CREATE PROCEDURE test_smalldatetime_date_p19 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-16' AS DATE) < smalldatetime_col;
GO

CREATE PROCEDURE test_smalldatetime_date_p20 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-16' AS DATE) <= smalldatetime_col;
GO

-- seq scan
CREATE VIEW test_smalldatetime_date_v1 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <> CAST('2023-06-13' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v2 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <> CAST('2023-06-15' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v3 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <> CAST('2023-06-17' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v4 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-13' AS DATE) <> smalldatetime_col;
GO

CREATE VIEW test_smalldatetime_date_v5 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-15' AS DATE) <> smalldatetime_col;
GO

CREATE VIEW test_smalldatetime_date_v6 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-17' AS DATE) <> smalldatetime_col;
GO

-- index scan
CREATE VIEW test_smalldatetime_date_v7 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col < CAST('2023-06-14' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v8 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <= CAST('2023-06-14' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v9 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col <= CAST('2023-06-18' AS DATE) AND smalldatetime_col >= CAST('2023-06-16' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v10 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col = CAST('2023-06-17' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v11 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col > CAST('2023-06-16' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v12 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col >= CAST('2023-06-16' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v13 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col BETWEEN CAST('2023-06-16' AS DATE) AND CAST('2023-06-18' AS DATE);
GO

CREATE VIEW test_smalldatetime_date_v14 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE smalldatetime_col IS NULL
GO

CREATE VIEW test_smalldatetime_date_v15 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-14' AS DATE) > smalldatetime_col;
GO

CREATE VIEW test_smalldatetime_date_v16 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-14' AS DATE) >= smalldatetime_col;
GO

CREATE VIEW test_smalldatetime_date_v17 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-16' AS DATE) <= smalldatetime_col AND CAST('2023-06-18' AS DATE) >= smalldatetime_col;
GO

CREATE VIEW test_smalldatetime_date_v18 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-17' AS DATE) = smalldatetime_col;
GO

CREATE VIEW test_smalldatetime_date_v19 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-16' AS DATE) < smalldatetime_col;
GO

CREATE VIEW test_smalldatetime_date_v20 AS
SELECT COUNT(*) FROM smalldate_date_cmp_t1 WHERE CAST('2023-06-16' AS DATE) <= smalldatetime_col;
GO

-- Creating index on date
-- FIX ME: Following test cases are for the index
-- created on date column, which will not choose 
-- index clause when comparison between date 
-- and smalldatetime.
CREATE TABLE date_smalldatetime_cmp_t1 (
    date_col DATE
)
GO

INSERT INTO date_smalldatetime_cmp_t1 (date_col) SELECT cast('2023-06-13' as date) from generate_series(1, 10)
GO

INSERT INTO date_smalldatetime_cmp_t1 (date_col) SELECT cast('2023-06-15' as date) from generate_series(1, 100000)
GO

INSERT INTO date_smalldatetime_cmp_t1 (date_col) SELECT cast('2023-06-17' as date) from generate_series(1, 10)
GO

CREATE NONCLUSTERED INDEX date_smalldatetime_cmp_ind1 ON date_smalldatetime_cmp_t1
(
    date_col ASC
)
GO

CREATE PROCEDURE test_date_smalldatetime_p1 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <> CAST('2023-06-13' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p2 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <> CAST('2023-06-15' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p3 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <> CAST('2023-06-17' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p4 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-13' AS SMALLDATETIME) <> date_col;
GO

CREATE PROCEDURE test_date_smalldatetime_p5 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-15' AS SMALLDATETIME) <> date_col;
GO

CREATE PROCEDURE test_date_smalldatetime_p6 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-17' AS SMALLDATETIME) <> date_col;
GO

CREATE PROCEDURE test_date_smalldatetime_p7 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col < CAST('2023-06-14' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p8 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <= CAST('2023-06-14' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p9 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <= CAST('2023-06-18' AS SMALLDATETIME) AND date_col >= CAST('2023-06-16' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p10 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col = CAST('2023-06-17' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p11 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col > CAST('2023-06-16' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p12 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col >= CAST('2023-06-16' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p13 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col BETWEEN CAST('2023-06-16' AS SMALLDATETIME) AND CAST('2023-06-18' AS SMALLDATETIME);
GO

CREATE PROCEDURE test_date_smalldatetime_p15 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-14' AS SMALLDATETIME) > date_col;
GO

CREATE PROCEDURE test_date_smalldatetime_p16 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-14' AS SMALLDATETIME) >= date_col;
GO

CREATE PROCEDURE test_date_smalldatetime_p17 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-16' AS SMALLDATETIME) <= date_col AND CAST('2023-06-18' AS SMALLDATETIME) >= date_col;
GO

CREATE PROCEDURE test_date_smalldatetime_p18 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-17' AS SMALLDATETIME) = date_col;
GO

CREATE PROCEDURE test_date_smalldatetime_p19 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-16' AS SMALLDATETIME) < date_col;
GO

CREATE PROCEDURE test_date_smalldatetime_p20 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-16' AS SMALLDATETIME) <= date_col;
GO

CREATE VIEW test_date_smalldatetime_v1 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <> CAST('2023-06-13' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v2 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <> CAST('2023-06-15' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v3 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <> CAST('2023-06-17' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v4 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-13' AS SMALLDATETIME) <> date_col;
GO

CREATE VIEW test_date_smalldatetime_v5 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-15' AS SMALLDATETIME) <> date_col;
GO

CREATE VIEW test_date_smalldatetime_v6 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-17' AS SMALLDATETIME) <> date_col;
GO

CREATE VIEW test_date_smalldatetime_v7 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col < CAST('2023-06-14' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v8 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <= CAST('2023-06-14' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v9 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col <= CAST('2023-06-18' AS SMALLDATETIME) AND date_col >= CAST('2023-06-16' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v10 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col = CAST('2023-06-17' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v11 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col > CAST('2023-06-16' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v12 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col >= CAST('2023-06-16' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v13 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE date_col BETWEEN CAST('2023-06-16' AS SMALLDATETIME) AND CAST('2023-06-18' AS SMALLDATETIME);
GO

CREATE VIEW test_date_smalldatetime_v15 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-14' AS SMALLDATETIME) > date_col;
GO

CREATE VIEW test_date_smalldatetime_v16 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-14' AS SMALLDATETIME) >= date_col;
GO

CREATE VIEW test_date_smalldatetime_v17 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-16' AS SMALLDATETIME) <= date_col AND CAST('2023-06-18' AS SMALLDATETIME) >= date_col;
GO

CREATE VIEW test_date_smalldatetime_v18 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-17' AS SMALLDATETIME) = date_col;
GO

CREATE VIEW test_date_smalldatetime_v19 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-16' AS SMALLDATETIME) < date_col;
GO

CREATE VIEW test_date_smalldatetime_v20 AS
SELECT COUNT(*) FROM date_smalldatetime_cmp_t1 WHERE CAST('2023-06-16' AS SMALLDATETIME) <= date_col;
GO

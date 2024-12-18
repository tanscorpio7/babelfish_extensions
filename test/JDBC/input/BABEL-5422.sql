-- Create tables
CREATE TABLE babel_5422_table (
    [primary] INT PRIMARY KEY,
    Name VARCHAR(50)
);
GO

CREATE TABLE babel_5422_table2 (
    [PRIMARY] INT,
    Age INT
);
GO

CREATE TABLE #babel_5422_table3 (
    [primary_column] INT,
    Age INT
);
GO

-- Insert sample data
INSERT INTO babel_5422_table ([primary], Name)
VALUES (1, 'John'), (2, 'Jane'), (3, 'Bob');
GO

INSERT INTO babel_5422_table2 ([primary], Age)
VALUES (1, 25), (3, 30);
GO

INSERT INTO #babel_5422_table3 ([primary_column], Age)
VALUES (1, 30), (3, 40);
GO

-- Aliasing
SELECT [primary] AS PrimaryKey, Name
FROM babel_5422_table;
GO

SELECT [primary_column] AS [Primary], Age
FROM #babel_5422_table3;
GO

-- Joining Tables
SELECT t1.[primary], t1.Name, t2.Age
FROM babel_5422_table t1
JOIN babel_5422_table2 t2 ON t1.[primary] = t2.[primary];
GO

-- Aggregation
SELECT COUNT([primary]) AS TotalRows
FROM babel_5422_table;
GO

-- Ordering
SELECT *
FROM babel_5422_table
ORDER BY [primary] DESC;
GO

-- Checking for Existence
IF EXISTS (SELECT 1 FROM babel_5422_table WHERE [primary] = 3)
    PRINT 'Row with [primary] = 3 exists';
ELSE
    PRINT 'Row with [primary] = 3 does not exist';
GO

-- Using in a CASE Statement
SELECT [primary],
    CASE
        WHEN [primary] = 1 THEN 'One'
        WHEN [primary] = 2 THEN 'Two'
        ELSE 'Other'
    END AS PrimaryDescription
FROM babel_5422_table;
GO

-- Using in a Subquery
SELECT Name
FROM babel_5422_table
WHERE [primary] IN (SELECT [primary] FROM babel_5422_table2);
GO

-- Using in a View
CREATE VIEW babel_5422_view
AS
SELECT [primary], Name
FROM babel_5422_table;
GO

SELECT * FROM babel_5422_view;
GO

-- Using in a Stored Procedure
CREATE PROCEDURE babel_5422_proc
    @PrimaryKey INT
AS
BEGIN
    SELECT [primary], Name
    FROM babel_5422_table
    WHERE [primary] = @PrimaryKey;
END
GO

EXEC babel_5422_proc 2;
GO

-- Using in a User-Defined Function
CREATE FUNCTION babel_5422_func
    (@PrimaryKey INT)
RETURNS TABLE
AS
RETURN(
    SELECT [primary], Name
    FROM babel_5422_table
    WHERE [primary] = @PrimaryKey
)
GO

SELECT babel_5422_func(3);
GO

-- Cleanup
DROP FUNCTION babel_5422_func;
GO

DROP PROCEDURE babel_5422_proc;
GO

DROP VIEW babel_5422_view;
GO

DROP TABLE #babel_5422_table3
GO

DROP TABLE babel_5422_table2;
GO

DROP TABLE babel_5422_table;
GO
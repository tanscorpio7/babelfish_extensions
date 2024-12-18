-- INSERT with OUTPUT
CREATE TABLE insert_table (ID INT IDENTITY(1,1), Name VARCHAR(50), Value INT);
GO

INSERT INTO insert_table (Name, Value) VALUES ('A', 10), ('B', 20), ('C', 30);
GO

DECLARE @ResultTable TABLE (NewID INT, NewName VARCHAR(50), NewValue INT);

INSERT INTO insert_table (Name, Value)
OUTPUT Inserted.ID, Inserted.Name, Inserted.Value INTO @ResultTable
SELECT Name, Value
FROM (VALUES ('D', 40), ('E', 50), ('F', 60), ('G', 70)) AS Source(Name, Value)
WHERE Value > 45;
GO

DROP TABLE insert_table;
GO

-- DELETE with OUTPUT
CREATE TABLE delete_table (id INT IDENTITY(1,1), name VARCHAR(50), Price DECIMAL(10,2));
GO

INSERT INTO delete_table (name, Price) VALUES ('Old Product 1', 99.99), ('Old Product 2', 149.99);
GO

CREATE TABLE #temp_delete_table (ID INT IDENTITY(1,1), DeletedID INT, DeletedName VARCHAR(50), DeletedPrice DECIMAL(10,2));
GO

DELETE FROM delete_table
OUTPUT Deleted.id, Deleted.name, Deleted.Price INTO #temp_delete_table
WHERE Price > 100;
GO

DROP TABLE delete_table;
DROP TABLE #temp_delete_table;
GO

-- UPDATE with OUTPUT
CREATE TABLE TestTable (ID INT, TEXTVal VARCHAR(100))
GO

INSERT INTO TestTable
VALUES (1, 'John'),
       (2, 'Jane'),
       (3, 'Bob'),
       (4, 'Alice');
GO

CREATE TABLE #TmpTable (
    c1 INT IDENTITY,
    ID_New INT,
    TEXTVal_New VARCHAR(100),
    ID_Old INT,
    TEXTVal_Old VARCHAR(100)
);
GO

UPDATE TestTable SET TEXTVal = 'NewValue'
OUTPUT Inserted.ID, Inserted.TEXTVal, Deleted.ID, Deleted.TEXTVal INTO #TmpTable WHERE ID IN (1,2)
GO

-- additional tests to prove IDENTITY_INSERT is not affected by the fix:
SET IDENTITY_INSERT #TmpTable ON
GO

UPDATE TestTable SET TEXTVal = 'NewValue'
OUTPUT Inserted.ID, Inserted.TEXTVal, Deleted.ID, Deleted.TEXTVal INTO #TmpTable WHERE ID IN (1,2)
GO

DROP TABLE TestTable;
DROP TABLE #TmpTable;
GO

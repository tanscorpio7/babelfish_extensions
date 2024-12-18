--
-- Tests on the result data type should retain as tsql base type after UNION
--
CREATE TABLE t1 (
  a INT,
  TransactionAmount money NULL
);
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 't1'
    AND a.[name] = 'TransactionAmount';
GO

CREATE VIEW TestView AS
    select
        traamo = a
    from t1
union all
    select
        traamo = TransactionAmount
    from t1;
GO

-- The result data type should retain as money
SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

CREATE VIEW TestView AS
  SELECT CASE 2
    WHEN 1 THEN a
    WHEN 2 THEN TransactionAmount
    END AS traamo
  FROM t1;
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';

GO

DROP VIEW TestView;
GO

CREATE VIEW TestView AS
    select
        traamo = null
    from t1
union all
    select
        traamo = TransactionAmount
    from t1;
GO

-- The result data type should retain as money
SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

CREATE VIEW TestView AS
    select
        traamo = CAST(null AS money)
    from t1
union all
    select
        traamo = TransactionAmount
    from t1;
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

CREATE VIEW TestView AS
    select
        traamo = CAST(11 AS NUMERIC)
    from t1
union all
    select
        traamo = TransactionAmount
    from t1;
GO

-- Result data type after UNION between numeric and money should be numeric
SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

DROP TABLE t1;
GO

CREATE TABLE t1 (
  a smallmoney,
  b money,
  c text,
  d numeric,
  e int
);
GO

INSERT INTO t1 VALUES (214748.3647, 922337203685477.5807, 'test', 1.2, 4), (-214748.3648, -922337203685477.5808, 'test', 2.3, 4);
GO

CREATE view TestView as
    select
        traamo = a
    from t1
union all
    select
        traamo = b
    from t1
union all
    select traamo = CAST(214748.3657 AS MONEY);
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

SELECT * FROM TestView ORDER BY traamo;
GO

DROP VIEW TestView;
GO

CREATE view TestView as
    select
        traamo = b
    from t1
union all
    select
        traamo = a
    from t1;
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

-- Coercion error should be raised
SELECT a FROM t1
UNION
SELECT b FROM t1
UNION
SELECT c FROM t1;
GO

CREATE view TestView as
    select
        traamo = a
    from t1
union all
    select
        traamo = b
    from t1
union all
    select
        traamo = d
    from t1
union all
    select
        traamo = e
    from t1;
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

DROP TABLE t1;
GO

CREATE TABLE ti (
  a tinyint,
  b smallint
);
GO

CREATE VIEW TestView AS
    select
        traamo = a
    from ti
union all
    select
        traamo = null
    from ti;
GO

-- Should be tinyint instead of smallint
SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

CREATE VIEW TestView AS
    select
        traamo = a
    from ti
union all
    select
        traamo = b
    from ti;
GO

-- Should be smallint since UNION between tinyint and smallint
SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

CREATE VIEW TestView AS
    select
        traamo = b
    from ti
union all
    select
        traamo = a
    from ti;
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

INSERT INTO ti VALUES (255, 256), (0, -1);
GO

SELECT * FROM TestView ORDER BY traamo;
GO

DROP VIEW TestView;

DROP TABLE ti;
GO

-- Involving user-defined type in T-SQL, result after UNION should be the base type
CREATE TYPE SSN FROM VARCHAR(11) NOT NULL;
GO

CREATE TABLE tn (a VARCHAR(11) NOT NULL, b SSN);
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'tn'
    AND a.[name] = 'b';
GO

CREATE VIEW TestView AS
    select
        traamo = 'hope'
    from tn
union all
    select
        traamo = b
    from tn;
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

CREATE VIEW TestView AS
    select
        traamo = a
    from tn
union all
    select
        traamo = b
    from tn;
GO

SELECT c.[name] AS [sys_types_user_type_id],
       TYPE_NAME(a.user_type_id) AS type_name_function_user_type_id
FROM sys.columns a
    INNER JOIN sys.objects b
        ON a.[object_id] = b.[object_id]
    LEFT JOIN sys.types c
        ON a.user_type_id = c.user_type_id
WHERE b.[name] = 'TestView'
    AND a.[name] = 'traamo';
GO

DROP VIEW TestView;
GO

DROP TABLE tn;
GO

DROP TYPE SSN;
GO

CREATE TYPE BABEL_CASE_EXPR_TEST_UDT FROM NVARCHAR(100);
GO

SELECT CASE 2
   WHEN 1 THEN CAST(0x123456 AS BABEL_CASE_EXPR_TEST_UDT)
   WHEN 2 THEN CAST(N'ةيسايق - ليجستلا ةقاطب' AS NTEXT)
END AS RESULT
GO

DROP TYPE BABEL_CASE_EXPR_TEST_UDT;
GO

-- Coercion error should be raised
SELECT 'unknown'
UNION
SELECT $1.0
UNION
SELECT N'xyz';
GO

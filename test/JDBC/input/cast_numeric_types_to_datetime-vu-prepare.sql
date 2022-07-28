USE master
GO

CREATE TABLE datetime_vu_prepare_table(c1 datetime)
GO

INSERT INTO datetime_vu_prepare_table VALUES 
(CAST(10 as DATETIME)), 
(CAST(CAST (1 as BIT) as DATETIME)), 
(CAST(CAST (216.5937465072345996348935531215926 as NUMERIC(19,7)) as DATETIME)), 
(CAST(CAST (-46.781289 as DECIMAL(6,2)) as DATETIME)), 
(CAST(CAST (54.63493295443232 as FLOAT(12)) as DATETIME)), 
(CAST(CAST (3.1215926 as REAL) as DATETIME)), 
(CAST(CAST (98 as INT) as DATETIME)), 
(CAST(CAST (131.23 as BIGINT) as DATETIME)), 
(CAST(CAST (5 as SMALLINT) as DATETIME)), 
(CAST(CAST (9 as TINYINT) as DATETIME))
GO

CREATE PROCEDURE datetime_vu_prepare_procedure 
AS
SELECT CAST(CAST (46.99999999 as MONEY) as DATETIME)
GO

CREATE FUNCTION dbo.datetime_vu_prepare_function (@par SMALLMONEY)
RETURNS DATETIME AS
BEGIN
    Declare @res datetime
	SELECT @res = (SELECT CAST(@par AS DATETIME))
    RETURN  @res
END;
GO
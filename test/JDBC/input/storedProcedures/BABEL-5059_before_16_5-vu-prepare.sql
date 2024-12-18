-- Procedure
-- nvarchar
CREATE PROCEDURE babel_5059_proc_test_1 (@a NVARCHAR, @b NVARCHAR OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_1_1 (@a NVARCHAR, @b NVARCHAR OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_1_2 (@a NVARCHAR, @b NVARCHAR OUTPUT) AS BEGIN SET @b=@a; SELECT len(@a); END;
GO

CREATE PROCEDURE babel_5059_proc_test_2 @a NVARCHAR(max) AS BEGIN SELECT @a; END;
GO

CREATE PROCEDURE babel_5059_proc_test_2_1 (@a NVARCHAR(max), @b NVARCHAR(max) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_3 (@a NVARCHAR(5), @b NVARCHAR(5) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_4 (@a NVARCHAR(50), @b NVARCHAR(50) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_5 (@a NVARCHAR(4000), @b NVARCHAR(4000) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

-- varchar
CREATE PROCEDURE babel_5059_proc_test_6 (@a VARCHAR, @b VARCHAR OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_7 (@a VARCHAR(max), @b VARCHAR(max) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_8 (@a VARCHAR(5), @b VARCHAR(5) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_9 (@a VARCHAR(50), @b VARCHAR(50) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_10 (@a VARCHAR(8000), @b VARCHAR(8000) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

-- varbinary
CREATE PROCEDURE babel_5059_proc_test_11 (@a VARBINARY, @b VARBINARY OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_11_1 (@a VARBINARY, @b VARBINARY OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_12 (@a VARBINARY(MAX), @b VARBINARY(MAX) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_13 (@a VARBINARY(27), @b VARBINARY(27) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_13_1 (@a VARBINARY(27), @b VARBINARY(27) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_14 (@a VARBINARY(8000), @b VARBINARY(8000) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO


-- nchar
CREATE PROCEDURE babel_5059_proc_test_15 (@a NCHAR, @b NCHAR OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_16 (@a NCHAR(10), @b NCHAR(10) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_17 (@a NCHAR(4000), @b NCHAR(4000) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO


-- smalldatetime
CREATE PROCEDURE babel_5059_proc_test_18 (@a smalldatetime, @b smalldatetime OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_19 (@a smalldatetime(2), @b smalldatetime(2) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_20 (@a smalldatetime(5), @b smalldatetime(5) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_21 (@a smalldatetime(6), @b smalldatetime(6) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

-- decimal
CREATE PROCEDURE babel_5059_proc_test_22 (@a decimal, @b decimal OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_23 (@a decimal(38,18), @b decimal(38,18) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO


-- binary
CREATE PROCEDURE babel_5059_proc_test_24 (@a binary, @b binary OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_25 (@a binary, @b binary OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_26 (@a binary(10), @b binary(10) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_27 (@a binary(8000), @b binary(8000) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

-- bpchar/char
CREATE PROCEDURE babel_5059_proc_test_28 (@a char, @b char OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE PROCEDURE babel_5059_proc_test_29 (@a char(10), @b char(10) OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_30 (@a char(8000), @b char(8000) OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

-- misc
CREATE PROCEDURE babel_5059_proc_test_main (@a smalldatetime, @b varchar OUTPUT) AS BEGIN SELECT @a;SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_main2 (@a varchar(max), @b varchar OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_main3 @Statement nvarchar(max) OUTPUT AS BEGIN
SET @Statement = 'SELECT * FROM sys.databases'; 
SELECT @Statement;
END;
GO

-- UDT testing
-- nvarchar
CREATE TYPE babel_5059_nvchar from nvarchar
GO

CREATE PROCEDURE babel_5059_proc_test_1_udt (@a babel_5059_nvchar, @b babel_5059_nvchar OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE TYPE babel_5059_nv_2 from nvarchar(2)
GO

CREATE PROCEDURE babel_5059_proc_test_2_udt @a babel_5059_nv_2 output AS BEGIN SELECT @a; END;
GO

CREATE TYPE babel_5059_nv_max from nvarchar(max)
GO

CREATE PROCEDURE babel_5059_proc_test_3_udt @a babel_5059_nv_max output AS BEGIN SELECT @a; END;
GO

-- varchar
CREATE TYPE babel_5059_vchar FROM varchar
GO

CREATE PROCEDURE babel_5059_proc_test_4_udt (@a babel_5059_vchar, @b babel_5059_vchar OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

CREATE TYPE babel_5059_vchar_2 FROM varchar(2)
GO

CREATE PROCEDURE babel_5059_proc_test_5_udt @a babel_5059_vchar_2 OUTPUT AS BEGIN SELECT @a; END;
GO

CREATE TYPE babel_5059_vchar_max from VARCHAR(max)
GO

CREATE PROCEDURE babel_5059_proc_test_6_udt (@a babel_5059_vchar_max, @b babel_5059_vchar_max OUTPUT) AS BEGIN SET @b=@a; SELECT len(@b); END;
GO

-- varbinary
CREATE TYPE babel_5059_varbinary FROM varbinary
GO

CREATE PROCEDURE babel_5059_proc_test_7_udt (@a babel_5059_varbinary, @b babel_5059_varbinary OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE TYPE babel_5059_varbinary_2 FROM varbinary(2)
GO

CREATE PROCEDURE babel_5059_proc_test_8_udt @a babel_5059_varbinary_2 OUTPUT AS BEGIN SELECT @a; END;
GO

CREATE TYPE babel_5059_varbinary_max FROM varbinary(max)
GO

CREATE PROCEDURE babel_5059_proc_test_9_udt @a babel_5059_varbinary_max OUTPUT AS BEGIN SELECT @a; END;
GO

-- nchar
CREATE TYPE babel_5059_nchar FROM nchar
GO

CREATE PROCEDURE babel_5059_proc_test_10_udt (@a babel_5059_nchar, @b babel_5059_nchar OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE TYPE babel_5059_nchar_2 FROM nchar(2)
GO

CREATE PROCEDURE babel_5059_proc_test_11_udt @a babel_5059_nchar_2 OUTPUT AS BEGIN SELECT @a; END;
GO

-- smalldatetime
CREATE TYPE babel_5059_smalldatetime FROM smalldatetime
GO

CREATE PROCEDURE babel_5059_proc_test_12_udt (@a babel_5059_smalldatetime, @b babel_5059_smalldatetime OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

-- decimal
CREATE TYPE babel_5059_decimal FROM decimal
GO

CREATE PROCEDURE babel_5059_proc_test_13_udt (@a babel_5059_decimal, @b babel_5059_decimal OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE TYPE babel_5059_decimal_10_2 FROM decimal(10,2)
GO

CREATE PROCEDURE babel_5059_proc_test_14_udt @a babel_5059_decimal_10_2 OUTPUT AS BEGIN SELECT @a; END;
GO

-- binary
CREATE TYPE babel_5059_binary FROM binary
GO

CREATE PROCEDURE babel_5059_proc_test_15_udt (@a babel_5059_binary, @b babel_5059_binary OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE TYPE babel_5059_binary_2 FROM binary(2)
GO

CREATE PROCEDURE babel_5059_proc_test_16_udt @a babel_5059_binary_2 OUTPUT AS BEGIN SELECT @a; END;
GO

-- char
CREATE TYPE babel_5059_char FROM char
GO

CREATE PROCEDURE babel_5059_proc_test_17_udt (@a babel_5059_char, @b babel_5059_char OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

CREATE TYPE babel_5059_char_2 FROM char(2)
GO

CREATE PROCEDURE babel_5059_proc_test_18_udt @a babel_5059_char_2 OUTPUT AS BEGIN SELECT @a; END;
GO

-- misc
CREATE PROCEDURE babel_5059_proc_test_19_udt (@a babel_5059_smalldatetime, @b babel_5059_vchar OUTPUT) AS BEGIN SELECT @a;SELECT @b; END;
GO

CREATE PROCEDURE babel_5059_proc_test_20_udt (@a babel_5059_vchar_max, @b babel_5059_vchar OUTPUT) AS BEGIN SET @b=@a; SELECT @b; END;
GO

-- Functions
-- NVARCHAR Variations
CREATE FUNCTION babel_5059_f1 (@p1 NVARCHAR) RETURNS NVARCHAR AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f2 (@p1 NVARCHAR(MAX)) RETURNS NVARCHAR(MAX) AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f3 (@p1 NVARCHAR(20)) RETURNS NVARCHAR(20) AS
BEGIN
    RETURN @p1
END;
GO

-- VARCHAR Variations
CREATE FUNCTION babel_5059_f4 (@p1 VARCHAR) RETURNS VARCHAR AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f5 (@p1 VARCHAR(MAX)) RETURNS VARCHAR(MAX) AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f6 (@p1 VARCHAR(20)) RETURNS VARCHAR(20) AS
BEGIN
    RETURN @p1
END;
GO

-- VARBINARY Variations
CREATE FUNCTION babel_5059_f7 (@p1 VARBINARY) RETURNS VARBINARY AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f8 (@p1 VARBINARY(MAX)) RETURNS VARBINARY(MAX) AS
BEGIN
    RETURN len(@p1)
END;
GO

CREATE FUNCTION babel_5059_f9 (@p1 VARBINARY(20)) RETURNS VARBINARY(20) AS
BEGIN
    RETURN @p1
END;
GO

-- NCHAR Variations
CREATE FUNCTION babel_5059_f10 (@p1 NCHAR) RETURNS NCHAR AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f11 (@p1 NCHAR(15)) RETURNS NCHAR(10) AS
BEGIN
    RETURN @p1
END;
GO

-- CHAR Variations
CREATE FUNCTION babel_5059_f12 (@p1 CHAR) RETURNS CHAR AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f13 (@p1 CHAR(10)) RETURNS CHAR(10) AS
BEGIN
    RETURN @p1
END;
GO

-- BINARY Variations
CREATE FUNCTION babel_5059_f14 (@p1 BINARY) RETURNS BINARY AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f15 (@p1 BINARY(10)) RETURNS BINARY(10) AS
BEGIN
    RETURN @p1
END;
GO

-- SMALLDATETIME Variations
CREATE FUNCTION babel_5059_f16 (@p1 SMALLDATETIME) RETURNS SMALLDATETIME AS
BEGIN
    RETURN @p1
END;
GO

-- DECIMAL Variations
CREATE FUNCTION babel_5059_f17 (@p1 DECIMAL) RETURNS DECIMAL AS
BEGIN
    RETURN @p1
END;
GO

CREATE FUNCTION babel_5059_f18 (@p1 DECIMAL(38, 18)) RETURNS DECIMAL(38, 18) AS
BEGIN
    RETURN @p1
END;
GO

-- combination

-- Input as VARCHAR, Output as VARCHAR
CREATE FUNCTION babel_5059_f19 (@p1 VARCHAR, @p2 VARCHAR(20))
RETURNS VARCHAR AS
BEGIN
    RETURN @p1 + ' ' + @p2
END;
GO

-- Input as VARCHAR(MAX), Output as VARCHAR(MAX)
CREATE FUNCTION babel_5059_f20 (@p1 VARCHAR(MAX), @p2 VARCHAR(20))
RETURNS VARCHAR(MAX) AS
BEGIN
    RETURN @p1 + ' ' + @p2
END;
GO

-- Input as VARCHAR, VARCHAR(MAX), Output as VARCHAR(20)
CREATE FUNCTION babel_5059_f21 (@p1 VARCHAR, @p2 VARCHAR(MAX))
RETURNS VARCHAR(20) AS
BEGIN
    RETURN LEFT(@p1 + ' ' + @p2, 20)
END;
GO

-- Input as VARCHAR(20), Output as VARCHAR(MAX)
CREATE FUNCTION babel_5059_f22 (@p1 VARCHAR(20), @p2 VARCHAR(20))
RETURNS VARCHAR(MAX) AS
BEGIN
    RETURN @p1 + ' ' + @p2
END;
GO

-- Input as VARCHAR(MAX), VARCHAR, Output as VARCHAR(20)
CREATE FUNCTION babel_5059_f23 (@p1 VARCHAR(MAX), @p2 VARCHAR)
RETURNS VARCHAR(20) AS
BEGIN
    RETURN LEFT(@p1 + ' ' + @p2, 20)
END;
GO

-- Input as VARBINARY, Output as VARCHAR(MAX)
CREATE FUNCTION babel_5059_f24 (@p1 VARBINARY(MAX))
RETURNS VARCHAR(MAX) AS
BEGIN
    RETURN CONVERT(VARCHAR(MAX), @p1, 2) -- Convert binary data to string (hex format)
END;
GO

-- Input as NCHAR, Output as VARCHAR
CREATE FUNCTION babel_5059_f25 (@p1 NCHAR(20))
RETURNS VARCHAR AS
BEGIN
    RETURN CONVERT(VARCHAR, @p1)
END;
GO

-- Input as DECIMAL, Output as VARCHAR(20)
CREATE FUNCTION babel_5059_f26 (@p1 DECIMAL(10,2))
RETURNS VARCHAR(20) AS
BEGIN
    RETURN CAST(@p1 AS VARCHAR(20))
END;
GO

-- Input as SMALLDATETIME, Output as VARCHAR
CREATE FUNCTION babel_5059_f27 (@p1 SMALLDATETIME)
RETURNS VARCHAR AS
BEGIN
    RETURN CONVERT(VARCHAR, @p1, 101) -- MM/DD/YYYY format
END;
GO

-- Input as VARBINARY, VARCHAR, Output as VARCHAR(MAX)
CREATE FUNCTION babel_5059_f28 (@p1 VARBINARY(MAX), @p2 VARCHAR(MAX))
RETURNS VARCHAR(MAX) AS
BEGIN
    RETURN CONVERT(VARCHAR(MAX), @p1, 2) + ' ' + @p2 -- Convert binary to string and concatenate
END;
GO

-- Input as VARCHAR(20), VARCHAR(MAX), Output as NCHAR(20)
CREATE FUNCTION babel_5059_f29 (@p1 VARCHAR(20), @p2 VARCHAR(MAX))
RETURNS NCHAR(20) AS
BEGIN
    RETURN LEFT(@p1 + ' ' + @p2, 20)
END;
GO

-- Input as VARCHAR, VARCHAR, Output as DECIMAL
CREATE FUNCTION babel_5059_f30 (@p1 VARCHAR, @p2 VARCHAR)
RETURNS DECIMAL(10, 2) AS
BEGIN
    RETURN CAST(@p1 AS DECIMAL(10, 2)) + CAST(@p2 AS DECIMAL(10, 2))
END;
GO

-- Input as VARBINARY(20), Output as VARCHAR(MAX)
CREATE FUNCTION babel_5059_f31 (@p1 VARBINARY(20))
RETURNS VARCHAR(MAX) AS
BEGIN
    RETURN CONVERT(VARCHAR(MAX), @p1, 2)
END;
GO

-- Input as VARCHAR, CHAR, Output as CHAR(10)
CREATE FUNCTION babel_5059_f32 (@p1 VARCHAR, @p2 CHAR(10))
RETURNS CHAR(10) AS
BEGIN
    RETURN LEFT(@p1 + ' ' + @p2, 10)
END;
GO

-- Input as BINARY(20), Output as CHAR(10)
CREATE FUNCTION babel_5059_f33 (@p1 BINARY(20))
RETURNS CHAR(10) AS
BEGIN
    RETURN LEFT(CONVERT(CHAR(20), @p1), 10)
END;
GO


-- UDT
-- VARCHAR types as input, VARCHAR(MAX) as output
CREATE FUNCTION babel_5059_udt_f1 (
    @p1 babel_5059_vchar,
    @p2 babel_5059_vchar_2,
    @p3 babel_5059_vchar_max
) RETURNS babel_5059_vchar_max
AS
BEGIN
    RETURN @p3
END;
GO

--  NVARCHAR types as input, NVARCHAR(MAX) as output
CREATE FUNCTION babel_5059_udt_f2 (
    @p1 babel_5059_nvchar,
    @p2 babel_5059_nv_2,
    @p3 babel_5059_nv_max
) RETURNS babel_5059_nv_max
AS
BEGIN
    RETURN @p3 
END;
GO

-- VARBINARY types as input, VARBINARY(MAX) as output
CREATE FUNCTION babel_5059_udt_f3 (
    @p1 babel_5059_varbinary,
    @p2 babel_5059_varbinary_2,
    @p3 babel_5059_varbinary_max
) RETURNS babel_5059_varbinary_max
AS
BEGIN
    RETURN @p3
END;
GO

--  NCHAR as input, NCHAR as output
CREATE FUNCTION babel_5059_udt_f4 (
    @p1 babel_5059_nchar,
    @p2 babel_5059_nchar_2
) RETURNS babel_5059_nchar
AS
BEGIN
    RETURN @p1
END;
GO

-- SMALLDATETIME as input, SMALLDATETIME as output
CREATE FUNCTION babel_5059_udt_f5 (
    @p1 babel_5059_smalldatetime
) RETURNS babel_5059_smalldatetime
AS
BEGIN
    RETURN @p1
END;
GO

-- DECIMAL types as input, DECIMAL(10,2) as output
CREATE FUNCTION babel_5059_udt_f6 (
    @p1 babel_5059_decimal,
    @p2 babel_5059_decimal_10_2
) RETURNS babel_5059_decimal_10_2
AS
BEGIN
    RETURN @p2
END;
GO

-- BINARY types as input, BINARY as output
CREATE FUNCTION babel_5059_udt_f7 (
    @p1 babel_5059_binary,
    @p2 babel_5059_binary_2
) RETURNS babel_5059_binary
AS
BEGIN
    RETURN @p1
END;
GO

-- CHAR types as input, CHAR(2) as output
CREATE FUNCTION babel_5059_udt_f8 (
    @p1 babel_5059_char,
    @p2 babel_5059_char_2
) RETURNS babel_5059_char_2
AS
BEGIN
    RETURN @p2
END;
GO

-- Mixed input types, VARBINARY(MAX) as output
CREATE FUNCTION babel_5059_udt_f9 (
    @p1 babel_5059_vchar,
    @p2 babel_5059_nchar,
    @p3 babel_5059_varbinary_max
) RETURNS babel_5059_varbinary_max
AS
BEGIN
    RETURN @p3
END;
GO

-- Mixed input types, DECIMAL(10,2) as output
CREATE FUNCTION babel_5059_udt_f10 (
    @p1 babel_5059_vchar,
    @p2 babel_5059_nvchar,
    @p3 babel_5059_decimal_10_2
) RETURNS babel_5059_decimal_10_2
AS
BEGIN
    RETURN @p3
END;
GO

-- Mixed input types, VARCHAR(MAX) as output
CREATE FUNCTION babel_5059_udt_f11 (
    @p1 babel_5059_binary,
    @p2 babel_5059_vchar_max
) RETURNS babel_5059_vchar_max
AS
BEGIN
    RETURN @p2
END;
GO

-- Mixed input types, NVARCHAR(MAX) as output
CREATE FUNCTION babel_5059_udt_f12 (
    @p1 babel_5059_varbinary_2,
    @p2 babel_5059_nv_max
) RETURNS babel_5059_nv_max
AS
BEGIN
    RETURN @p2
END;
GO

-- Mixed input types, CHAR as output
CREATE FUNCTION babel_5059_udt_f13 (
    @p1 babel_5059_decimal,
    @p2 babel_5059_char_2
) RETURNS babel_5059_char
AS
BEGIN
    RETURN @p2
END;
GO

-- VARCHAR types as input, VARCHAR(MAX) as output
CREATE FUNCTION babel_5059_udt_f14 (
    @p1 babel_5059_vchar,
    @p2 babel_5059_vchar_2,
    @p3 babel_5059_vchar_max
) RETURNS babel_5059_vchar_max
AS
BEGIN
    RETURN @p3 END;
GO

CREATE FUNCTION babel_5059_udt_f15 (
    @p1 babel_5059_vchar,
    @p2 babel_5059_vchar_2,
    @p3 babel_5059_vchar_max
) RETURNS babel_5059_vchar_max
AS
BEGIN
    RETURN @p1 END;
GO

-- VARCHAR types as input, VARCHAR as output
CREATE FUNCTION babel_5059_udt_f16 (
    @p1 babel_5059_vchar,
    @p2 babel_5059_vchar_2,
    @p3 babel_5059_vchar_max
) RETURNS babel_5059_vchar
AS
BEGIN
    RETURN @p3 END;
GO

-- ITVF , our change does not have any impact on inline table valued functions, thus adding only a few tests
CREATE TABLE babel_5059_t1(col_char char(20),group_id int,col_nvarchar nvarchar(50))
GO

INSERT INTO babel_5059_t1 VALUES( N'abc', 1,  N'defghi')
GO

CREATE FUNCTION babel_5059_itvf_func1()
RETURNS TABLE
AS
    RETURN (SELECT col_nvarchar FROM babel_5059_t1)
GO

CREATE FUNCTION babel_5059_itvf_func2()
RETURNS TABLE
AS
    RETURN (SELECT group_id,col_nvarchar FROM babel_5059_t1 GROUP BY group_id,col_nvarchar ORDER BY group_id)
GO

CREATE TABLE babel_5059_vu_prepare_t1(number int)
GO

INSERT INTO babel_5059_vu_prepare_t1 VALUES(5)
GO

CREATE FUNCTION babel_5059_vu_prepare_f3()
returns table
AS
    RETURN (select '|' + SPACE(number) + '|' as result from babel_5059_vu_prepare_t1)
GO

-- multi statement table valued function

CREATE TABLE babel_5059_t2 (customer_id int primary key,contactname varchar(50) not null,city varchar(20))
GO

INSERT INTO babel_5059_t2(customer_id,contactname,city)
values(1,'Maria Anders','Berlin'), (2,'Ana Trujillo','London'),(3,'Antonio Moreno','Lulea'),
(4,'Thomas Hardy','Madrid'),(5,'Hanna Moos','London'),(6,'Yang Wang','Ben')
GO

CREATE TABLE babel_5059_t3(order_id int primary key,customer_id int,order_date date,country varchar(50))
GO

INSERT INTO babel_5059_t3(order_id,customer_id,order_date,country)
values(10252,4,'1996-07-09 00:00:00.000','Belgium'),(10253,3,'1996-07-10 00:00:00.000','Brazil'),(10254,5,'1996-07-11 00:00:00.000','Switzerland'),
(10255,9,'1996-07-12 00:00:00.000','Switzerland'),(10256,3,'1996-07-15 00:00:00.000','Brazil'),(10257,4,'1996-07-16 00:00:00.000','Austria')
GO

CREATE FUNCTION babel_5059_vu_prepare_mstvf_1 ()
RETURNS @t TABLE (CustomerID int, ContactName nvarchar(50), Orderid int,orderdate date,city varchar(50))
AS
BEGIN
    INSERT INTO @t SELECT c.customer_id, c.ContactName, order_id ,order_date,city FROM babel_5059_t2 c JOIN babel_5059_t3 o ON c.customer_id = o.customer_id 
RETURN
END
GO

CREATE FUNCTION babel_5059_vu_prepare_mstvf_2(@i int) 
returns @tableVar table (a nvarchar(10), b int, c int)
as
begin
    insert into @tableVar values('hello1', 1, 100);
    insert into @tableVar values('hello2', 2, 200);
    insert into @tableVar values('hello3', 3, 300);
    update @tableVar set b = 2 where b = 3;
    delete @tableVar where b = 2;
return;
end;
GO

-- to do in hashbytes pr
-- -- TEST CASE 1: creating a table and then calling hashbytes for nvarchar and varchar input
-- insert into TestHash values('value1', 'value1');
-- GO

-- SELECT DATALENGTH( nvarchar_data) as nvarchar_data_datalength
--         , LEN( nvarchar_data) AS nvarchar_data_len
--         , DATALENGTH( varchar_data) as varchar_data_btyes_datalength
--         , LEN( varchar_data) AS varchar_data_len
--         , * 
-- from TestHash;
-- GO
-- -- TEST CASE 2: Casting nvarchar and varchar with different algorithms using Hashbytes
-- SELECT hashbytes( 'sha1', 'test string' ) as vary_string, hashbytes( 'sha1', N'test string' ) as unicode_string
-- GO

-- SELECT hashbytes( 'MD2', 'test string' ) as vary_string, hashbytes( 'MD2', N'test string' ) as unicode_string
-- GO

-- SELECT hashbytes( 'MD4', 'test string' ) as vary_string, hashbytes( 'MD4', N'test string' ) as unicode_string
-- GO

-- SELECT hashbytes( 'MD5', 'test string' ) as vary_string, hashbytes( 'MD5', N'test string' ) as unicode_string
-- GO

-- SELECT hashbytes( 'SHA2_256', 'test string' ) as vary_string, hashbytes( 'SHA2_256', N'test string' ) as unicode_string
-- GO

-- SELECT hashbytes( 'SHA2_512', 'test string' ) as vary_string, hashbytes( 'SHA2_512', N'test string' ) as unicode_string
-- GO

-- SELECT hashbytes( 'SHA', 'test string' ) as vary_string, hashbytes( 'SHA', N'test string' ) as unicode_string
-- GO

-- SELECT hashbytes( 'sha1', 'test string')
-- GO
-- --TEST CASE 3: testing hashbytes via casting a varchar to nvarchar 
-- SELECT hashbytes('sha1',cast('test string' as sys.nvarchar))
-- GO

--TEST CASE 4: Casting function for nvarchar to varbinary

SELECT cast(N'test string' as varbinary);
GO

SELECT cast(cast(cast('ab' AS nvarchar(10)) as varbinary(2)) as nvarchar(2));
GO

-- to do in hashbytes PR
-- -- Test Case 5: Empty strings and NULL values
-- SELECT HASHBYTES('SHA1', '') AS empty_varchar,
--        HASHBYTES('SHA1', N'') AS empty_nvarchar,
--        HASHBYTES('SHA1', NULL) AS null_input;
-- GO

-- -- Test Case 6: Unicode characters
-- SELECT HASHBYTES('SHA1', N'こんにちは') AS japanese,
--        HASHBYTES('SHA1', N'Здравствуйте') AS russian,
--        HASHBYTES('SHA1', N'🙂😊😀') AS emojis;
-- GO

-- -- Test Case 8: Different collations
-- SELECT HASHBYTES('SHA1', N'hello' COLLATE Latin1_General_CI_AS) AS ci_as,
--        HASHBYTES('SHA1', N'hello' COLLATE Latin1_General_CS_AS) AS cs_as;
-- GO

-- -- Test Case 9: Combining varchar and nvarchar
-- SELECT HASHBYTES('SHA1', 'hello' + N'world') AS combined;
-- GO

-- Test Case 10: Roundtrip conversions
SELECT CAST(CAST(N'test' AS VARBINARY(8)) AS NVARCHAR(4)) AS nvarchar_roundtrip,
       CAST(CAST('test' AS VARBINARY(8)) AS VARCHAR(4)) AS varchar_roundtrip;
GO

-- Test Case 11: CAST and CONVERT between varchar, nvarchar, and varbinary
SELECT CAST(N'hello' AS VARBINARY(10)) AS nvarchar_to_varbinary,
       CAST('hello' AS VARBINARY(10)) AS varchar_to_varbinary,
       CONVERT(VARBINARY(10), N'hello') AS nvarchar_to_varbinary_convert,
       CONVERT(VARBINARY(10), 'hello') AS varchar_to_varbinary_convert;
GO

-- to do in hashbytes PR
-- -- Test Case 12: Testing with special characters
-- SELECT HASHBYTES('SHA1', 'Hello, World!') AS varchar_special,
--        HASHBYTES('SHA1', N'Hello, World!') AS nvarchar_special,
--        HASHBYTES('SHA1', 'Hello' + CHAR(13) + CHAR(10) + 'World') AS varchar_newline,
--        HASHBYTES('SHA1', N'Hello' + NCHAR(13) + NCHAR(10) + N'World') AS nvarchar_newline;
-- GO

-- Test Case 13: NVARCHAR -> VARBINARY -> VARCHAR -> NVARCHAR -> VARBINARY
SELECT CAST(
    CAST(
        CAST(
            CAST(N'Hello World' AS VARBINARY(100))
        AS VARCHAR(100))
    AS NVARCHAR(100))
AS VARBINARY(100)) AS four_casts;
GO

-- Test Case 14: VARCHAR -> NVARCHAR -> VARBINARY -> NVARCHAR -> VARCHAR
SELECT CAST(
    CAST(
        CAST(
            CAST('Test String' AS NVARCHAR(50))
        AS VARBINARY(50))
    AS NVARCHAR(50))
AS VARCHAR(50)) AS four_casts;
GO

-- Test Case 15: Implicit conversion from data type nvarchar to varbinary is not allowed.
DECLARE @a varbinary(10); SET @a = CAST(N'21' AS nvarchar(10)); SELECT @a
GO

-- Test Case 16: Casting with UDT on nvarchar -> varbinary, UDT on varbinary to nvarchar, UDT on nvarchar to UDT on varbinary
create type user_defined_nvarchar from nvarchar(50);
select cast(cast(N'test string' as user_defined_nvarchar) as varbinary)
GO

create type user_defined_varbinary from varbinary(50);
select cast(cast(0x7400650073007400200073007400720069006E006700 as user_defined_varbinary) as nvarchar)
GO

select cast(cast(N'test string' as user_defined_nvarchar) as user_defined_varbinary)
go
-- Test Case 17: NVARCHAR-> BINARY
select cast(N'test string' as binary)
GO
-- Test Case 18: NVARCHAR-> User Defined varbinary
select cast(N'test string' as user_defined_varbinary)
GO

-- to do in hashbytes PR
-- -- Test Case 19: User defined hashbytes function
-- create function dbo.hashbytes(@data sys.varchar)returns sys.varchar AS BEGIN    return "dummy hashbytes";END
-- GO
-- SELECT hashbytes( 'SHA', 'test string' ) as vary_string, hashbytes( 'SHA', N'test string' ) as unicode_string
-- GO
-- select hashbytes('abc')
-- GO
-- drop function dbo.hashbytes
-- GO

-- Test 20: Calling the function, procedure, views from prepare scripts

-- Test CastNVarcharToVarbinary function
DECLARE @TestString NVARCHAR(100) = N'Test String';
DECLARE @BinaryResult VARBINARY(MAX);
SET @BinaryResult = dbo.CastNVarcharToVarbinary(@TestString);
select cast (@BinaryResult as nvarchar)
GO

-- Test CastVarbinaryToNVarchar function

DECLARE @TestBinary VARBINARY(100) = 0x54657374537472696E67; -- 'Test String' in ASCII
DECLARE @StringResult NVARCHAR(MAX);
SET @StringResult = dbo.CastVarbinaryToNVarchar(@TestBinary);
select cast (@StringResult as nvarchar)
GO

-- Test CastbinaryToNVarchar function

DECLARE @TestBinary BINARY = 0x54657374537472696E67; -- 'Test String' in ASCII
DECLARE @StringResult NVARCHAR(MAX);
SET @StringResult = dbo.CastbinaryToNVarchar(@TestBinary);
select cast (@StringResult as nvarchar)
GO

-- Test CastDemoView
DECLARE @ViewResult TABLE (
    NVarcharToVarbinary VARBINARY(MAX),
    VarbinaryToNVarchar NVARCHAR(MAX),
    binaryToNVarchar NVARCHAR(MAX)
);

INSERT INTO @ViewResult
SELECT * FROM dbo.CastDemoView;
go

SELECT * FROM dbo.CastDemoView1;
GO
-- TEST CASE 21: Assigned Casting
DECLARE @NVarcharValue NVARCHAR(100) = N'Hello, World!';
DECLARE @AssignedVarbinary VARBINARY(100);
SET @AssignedVarbinary = CAST(@NVarcharValue AS VARBINARY(100));
select CAST(@AssignedVarbinary AS NVARCHAR(100));
GO

--TEST CASE 22: Calling hashbytes via function, procedure, views

-- Test the HashMultipleTypes function
DECLARE @TestVarchar VARCHAR(10) = 'Test';
DECLARE @TestNVarchar NVARCHAR(10) = N'Test';
DECLARE @TestVarbinary VARBINARY(10) = 0x54657374; -- 'Test' in ASCII

-- to do in hashbytes PR
-- SELECT * FROM dbo.HashMultipleTypes(@TestVarchar, @TestNVarchar, @TestVarbinary);
-- GO
-- -- Test the HashDemoView
-- SELECT * FROM dbo.HashDemoView;
-- GO

--TEST CASE 23: BINARY TO NVARCHAR
select cast(cast(N'test string' as binary) as nvarchar)
GO
--TEST CASE 24: NVARCHAR TO BINARY with maxlen < length of actual string
select cast(0x610061006100610061006100610061006100 as nvarchar(5))
GO
select cast(0x8765 as nvarchar(1))
GO

--TEST CASE 25: Default typmod case with string getting truncated

select cast(N'Lorem ipsum dolor sit amet' as varbinary)
GO
select cast(0x4C006F00720065006D00200069007000730075006D00200064006F006C00 as nvarchar)
GO
--TEST CASE 26: String getting truncted with defined typmod i.e len > maxlen
select cast(N'tesst string' as varbinary(4))
GO
select cast(0x74006500 as nvarchar)
GO

select cast(N'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea' as varbinary(MAX))
GO

select cast(0x0006161 as nvarchar)
GO

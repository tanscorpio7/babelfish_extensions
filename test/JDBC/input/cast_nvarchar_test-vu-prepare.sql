-- to do in hashbytes PR
-- CREATE TABLE TestHash(
-- nvarchar_data nvarchar(32) NOT NULL,
-- varchar_data varchar(32) NOT NULL,
-- cast_hashbytes_nvarchar_data  AS (cast ( hashbytes('sha1', nvarchar_data) AS varbinary(20) )) PERSISTED NOT NULL,
-- convert_hashbytes_nvarchar_data AS (convert( varbinary(20),  hashbytes('sha1',nvarchar_data))) PERSISTED NOT NULL,
-- cast_hashbytes_varchar_data  AS (cast ( hashbytes('sha1', varchar_data) AS varbinary(20) )) PERSISTED NOT NULL,
-- convert_hashbytes_varchar_data AS (convert( varbinary(20),  hashbytes('sha1',varchar_data))) PERSISTED NOT NULL
-- );
-- GO

-- Function to cast NVARCHAR to VARBINARY
CREATE FUNCTION dbo.CastNVarcharToVarbinary
(
    @Input NVARCHAR(MAX)
)
RETURNS VARBINARY(MAX)
AS
BEGIN
    RETURN CAST(@Input AS VARBINARY(MAX));
END
GO

-- Function to cast VARBINARY to NVARCHAR
CREATE FUNCTION dbo.CastVarbinaryToNVarchar
(
    @Input VARBINARY(MAX)
)
RETURNS NVARCHAR(MAX)
AS
BEGIN
    RETURN CAST(@Input AS NVARCHAR(MAX));
END
GO

-- Function to cast BINARY to NVARCHAR
CREATE FUNCTION dbo.CastbinaryToNVarchar
(
    @Input BINARY
)
RETURNS NVARCHAR(MAX)
AS
BEGIN
    RETURN CAST(@Input AS NVARCHAR(MAX));
END
GO

-- View that demonstrates both casts
CREATE VIEW dbo.CastDemoView
AS
SELECT 
    dbo.CastNVarcharToVarbinary(N'Hello, World!') AS NVarcharToVarbinary,
    dbo.CastVarbinaryToNVarchar(0x48656C6C6F2C20576F726C6421) AS VarbinaryToNVarchar,
    dbo.CastbinaryToNVarchar(0x48656C6C6F2C20576F726C6421) AS binaryToNVarchar
GO

create table casttable(nvarchar_data nvarchar(max), varbinary_data varbinary(max), binary_data binary(15), nvarchar_binary_data nvarchar(max));
GO
insert into casttable(nvarchar_data, varbinary_data, binary_data, nvarchar_binary_data) values (N'test string', 0x48656C6C6F2C20576F726C6421, 0x48656C6C6F2C20576F726C6421, N'test string')
GO

CREATE VIEW dbo.CastDemoView1
AS
SELECT
    CAST(nvarchar_data AS VARBINARY(MAX)),
    CAST(varbinary_data AS NVARCHAR(MAX)),
    CAST(binary_data AS NVARCHAR(MAX)),
    CAST(nvarchar_binary_data AS BINARY(15))
FROM casttable
GO

-- to do in hashbytes PR
-- -- Function  hashbytes different input types
-- CREATE FUNCTION dbo.HashMultipleTypes
-- (
--     @VarcharInput VARCHAR(MAX),
--     @NVarcharInput NVARCHAR(MAX),
--     @VarbinaryInput VARBINARY(MAX)
-- )
-- RETURNS TABLE
-- AS
-- RETURN
-- (
--     SELECT 
--         HASHBYTES('SHA2_256', @VarcharInput) AS VarcharHash,
--         HASHBYTES('SHA2_256', @NVarcharInput) AS NVarcharHash,
--         HASHBYTES('SHA2_256', @VarbinaryInput) AS VarbinaryHash
-- )
-- GO


-- -- View to demonstrate hashing
-- CREATE VIEW dbo.HashDemoView
-- AS
-- SELECT 
--     HASHBYTES('SHA2_256', CAST('Hello' AS VARCHAR(MAX))) AS VarcharHash,
--     HASHBYTES('SHA2_256', CAST(N'Hello' AS NVARCHAR(MAX))) AS NVarcharHash,
--     HASHBYTES('SHA2_256', CAST(0x48656C6C6F AS VARBINARY(MAX))) AS VarbinaryHash
-- GO


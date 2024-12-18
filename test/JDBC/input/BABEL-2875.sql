CREATE TYPE type_int FROM INT NOT NULL
go

CREATE TABLE t_sksql(
        c1      int not null
        , c2 type_int
)
go

SELECT SUBSTRING( T.name, 1, 20 ) AS "UDDT name",
SUBSTRING( T2.name, 1, 20 ) AS "system dt name",
OBJECT_NAME(T.system_type_id) AS "System type name", -- system_type_id is not static
OBJECT_NAME(T.user_type_id) AS "User type name" -- user_type_id is not static
FROM sys.columns C, sys.types T, sys.types T2
WHERE C.object_id = OBJECT_ID( 't_sksql' )
AND C.user_type_id = T.user_type_id
AND T.system_type_id = T2.user_type_id
ORDER BY T.name
go

-- checking if UDDTs leak through to other databases
Create database BABEL2875
go

Use BABEL2875
go

Select count(*) from sys.types where name = 'type_int'
go

use master
go

drop database BABEL2875
go

-- Tests for db level collation
-- checking if UDDTs leak through to other databases
Create database BABEL2875 COLLATE BBF_Unicode_CP1_CI_AI
go

Use BABEL2875
go

Select count(*) from sys.types where name = 'type_int'
go

use master
go

drop table t_sksql
drop type type_int
drop database BABEL2875
go
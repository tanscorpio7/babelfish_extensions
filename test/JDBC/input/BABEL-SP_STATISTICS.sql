create database db1
go
use db1
go
create table t1(a int)
go
create index i1 on t1(a)
go
create table t2(a int, b int)
go
create index i2 on t2(a,b)
go
create table t3(a int, b int, c int)
go
create index i3 on t3(c,a)
go
CREATE TABLE t4(
        c1 INT PRIMARY KEY
        , c2 CHAR(10) NOT NULL UNIQUE
        , c3 VARCHAR(20) NULL
)
create index i4 on t4(c2)
go

create table t5(a int)
go

-- syntax error: @table_name is required
exec sp_statistics
go

exec sp_statistics @table_name = 't1'
go

exec sp_statistics @table_name = 't2', @table_qualifier = 'db1'
go

exec sp_statistics @table_name = 't3', @table_owner = 'dbo'
go

exec sp_statistics @table_name = 't4'
go

exec sp_statistics @table_name = 't4', @is_unique = 'Y'
go

exec [sys].sp_statistics @table_name = 't5'
go

-- unnamed invocation
exec sp_statistics 't1', 'dbo', 'db1'
go

-- case-insensative invocation
EXEC sp_statistics @TABLE_NAME = 't2', @TABLE_OWNER = 'dbo', @TABLE_QUALIFIER = 'db1'
GO

exec sp_statistics N't1',N'dbo',NULL,N'%',N'Y',N'Q'
go

-- sp_statistics_100 is implemented as same as sp_statistics
exec sp_statistics_100 @table_name = 't3' 
go

drop index i1 on t1
go
drop index i2 on t2
go
drop index i3 on t3
go
drop index i4 on t4
go
drop table t1
go
drop table t2
go
drop table t3
go
drop table t4
go
drop table t5
go
use master
go
drop database db1
go

create database db1 COLLATE bbf_unicode_cp1_ci_ai;
go
use db1
go
create table t1(a int)
go
create index i1 on t1(a)
go
create table t2(a int, b int)
go
create index i2 on t2(a,b)
go
create table t3(a int, b int, c int)
go
create index i3 on t3(c,a)
go
CREATE TABLE t4(
        c1 INT PRIMARY KEY
        , c2 CHAR(10) NOT NULL UNIQUE
        , c3 VARCHAR(20) NULL
)
create index i4 on t4(c2)
go

create table t5(a int)
go

-- syntax error: @table_name is required
exec sp_statistics
go

exec sp_statistics @table_name = 't1'
go

exec sp_statistics @table_name = 't2', @table_qualifier = 'db1'
go

exec sp_statistics @table_name = 't3', @table_owner = 'dbo'
go

exec sp_statistics @table_name = 't4'
go

exec sp_statistics @table_name = 't4', @is_unique = 'Y'
go

exec [sys].sp_statistics @table_name = 't5'
go

-- unnamed invocation
exec sp_statistics 't1', 'dbo', 'db1'
go

-- case-insensative invocation
EXEC sp_statistics @TABLE_NAME = 't2', @TABLE_OWNER = 'dbo', @TABLE_QUALIFIER = 'db1'
GO

exec sp_statistics N't1',N'dbo',NULL,N'%',N'Y',N'Q'
go

-- sp_statistics_100 is implemented as same as sp_statistics
exec sp_statistics_100 @table_name = 't3' 
go

drop index i1 on t1
go
drop index i2 on t2
go
drop index i3 on t3
go
drop index i4 on t4
go
drop table t1
go
drop table t2
go
drop table t3
go
drop table t4
go
drop table t5
go
use master
go
drop database db1
go

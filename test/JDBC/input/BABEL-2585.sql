USE master;
go

CREATE DATABASE [babelfish-1234];
go

DROP DATABASE [babelfish-1234];
go

-- Tests for db level collation
CREATE DATABASE [babelfish-1234] COLLATE BBF_Unicode_CP1_CI_AI;
go

DROP DATABASE [babelfish-1234];
go

CREATE DATABASE babel_recursive_cte;
go

DROP DATABASE babel_recursive_cte;
go

-- Tests for db level collation
CREATE DATABASE babel_recursive_cte COLLATE BBF_Unicode_CP1_CI_AI;
go

DROP DATABASE babel_recursive_cte;
go

CREATE SCHEMA [babelfish-1234];
go

DROP SCHEMA [babelfish-1234];
go

CREATE SCHEMA babel_recursive_cte;
go

DROP SCHEMA babel_recursive_cte;
go

-- Test for BABEL-2589
-- Check the stability of CREATE/DROP LOGIN with different names
CREATE LOGIN babel_2589_login_1 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_1
go

CREATE LOGIN babel_2589_login_2 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_2
go

CREATE LOGIN babel_2589_login_3 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_3
go

CREATE LOGIN babel_2589_login_4 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_4
go

CREATE LOGIN babel_2589_login_5 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_5
go

CREATE LOGIN babel_2589_login_6 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_6
go

CREATE LOGIN babel_2589_login_7 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_7
go

CREATE LOGIN babel_2589_login_8 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_8
go

CREATE LOGIN babel_2589_login_9 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_9
go

CREATE LOGIN babel_2589_login_10 WITH PASSWORD = '123'
go
DROP LOGIN babel_2589_login_10
go

-- Test for BABEL-2600
-- Check stability of CREATE/DROP DATABASE with different names
CREATE DATABASE babel_2600_db_1
go
USE babel_2600_db_1
go
USE master
go
DROP DATABASE babel_2600_db_1
go

-- Tests for db level collation
CREATE DATABASE babel_2600_db_1 COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db_1
go
USE master
go
DROP DATABASE babel_2600_db_1
go

CREATE DATABASE babel_2600_db_2
go
USE babel_2600_db_2
go
USE master
go
DROP DATABASE babel_2600_db_2
go

-- Tests for db level collation
CREATE DATABASE babel_2600_db_2 COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db_2
go
USE master
go
DROP DATABASE babel_2600_db_2
go

CREATE DATABASE babel_2600_db_3
go
USE babel_2600_db_3
go
USE master
go
DROP DATABASE babel_2600_db_3
go

-- Tests for db level collation
CREATE DATABASE babel_2600_db_3 COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db_3
go
USE master
go
DROP DATABASE babel_2600_db_3
go

CREATE DATABASE babel_2600_db_4
go
USE babel_2600_db_4
go
USE master
go
DROP DATABASE babel_2600_db_4
go

-- Tests for db level collation
CREATE DATABASE babel_2600_db_4 COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db_4
go
USE master
go
DROP DATABASE babel_2600_db_4
go

CREATE DATABASE babel_2600_db_5
go
USE babel_2600_db_5
go
USE master
go
DROP DATABASE babel_2600_db_5
go

-- Tests for db level collation
CREATE DATABASE babel_2600_db_5 COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db_5
go
USE master
go
DROP DATABASE babel_2600_db_5
go

-- Check stability of CREATE/DROP DATABASE with the same name
CREATE DATABASE babel_2600_db
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

CREATE DATABASE babel_2600_db
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

CREATE DATABASE babel_2600_db
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

CREATE DATABASE babel_2600_db
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

CREATE DATABASE babel_2600_db
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

-- Tests for db level collation
CREATE DATABASE babel_2600_db COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

CREATE DATABASE babel_2600_db COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

CREATE DATABASE babel_2600_db COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

CREATE DATABASE babel_2600_db COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

CREATE DATABASE babel_2600_db COLLATE BBF_Unicode_CP1_CI_AI
go
USE babel_2600_db
go
USE master
go
DROP DATABASE babel_2600_db
go

-- Test for BABEL-2586
-- Check if we can get the correct default db after multiple operations
DECLARE @default_db VARCHAR(10)
SET @default_db = sys.babelfish_get_login_default_db('jdbc_user');
SELECT CASE WHEN @default_db = 'master' THEN 'correct' ELSE 'error' END;
go

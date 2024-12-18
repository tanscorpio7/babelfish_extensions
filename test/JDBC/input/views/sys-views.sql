CREATE DATABASE db1;
GO

USE db1
GO

CREATE VIEW rand_name1 AS select 1;
GO

SELECT COUNT(*) FROM sys.views WHERE name = 'rand_name1';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1' and type='V';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1' and type='v';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1' and type_desc='VIEW';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1' and type_desc='view';
GO

SELECT COUNT(*) FROM sys.objects WHERE type='V' and name = 'rand_name1';
GO

SELECT COUNT(*) FROM sys.all_objects WHERE type='V' and name = 'rand_name1';
GO

USE master;
GO

#view rand_name1 should not be visible in master database.
SELECT COUNT(*) FROM sys.views WHERE name = 'rand_name1';
GO

SELECT COUNT(*) FROM sys.objects WHERE type='V' and name = 'rand_name1';
GO

SELECT COUNT(*) FROM sys.all_objects WHERE type='V' and name = 'rand_name1';
GO

CREATE VIEW rand_name2 AS select 1;
GO

SELECT COUNT(*) FROM sys.views WHERE name = 'rand_name2';
GO

SELECT COUNT(*) FROM sys.objects WHERE type='V' and name = 'rand_name2';
GO

SELECT COUNT(*) FROM sys.all_objects WHERE type='V' and name = 'rand_name2';
GO

USE db1
GO

#view rand_name2 should not be visible in db1 database.
SELECT COUNT(*) FROM sys.views WHERE name = 'rand_name2';
GO

SELECT COUNT(*) FROM sys.objects WHERE type='V' and name = 'rand_name2';
GO

SELECT COUNT(*) FROM sys.all_objects WHERE type='V' and name = 'rand_name2';
GO

DROP VIEW rand_name1;
GO

USE master;
GO

DROP DATABASE db1;
GO

DROP VIEW rand_name2;
GO

CREATE DATABASE db1 COLLATE BBF_Unicode_CP1_CI_AI;
GO

USE db1
GO

CREATE VIEW rand_name1 AS select 1;
GO

SELECT COUNT(*) FROM sys.views WHERE name = 'rand_name1';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1' and type='V';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1' and type='v';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1' and type_desc='VIEW';
GO

SELECT count(*) FROM sys.views WHERE name='RAND_nAME1' and type_desc='view';
GO

SELECT COUNT(*) FROM sys.objects WHERE type='V' and name = 'rand_name1';
GO

SELECT COUNT(*) FROM sys.all_objects WHERE type='V' and name = 'rand_name1';
GO

USE master;
GO

#view rand_name1 should not be visible in master database.
SELECT COUNT(*) FROM sys.views WHERE name = 'rand_name1';
GO

SELECT COUNT(*) FROM sys.objects WHERE type='V' and name = 'rand_name1';
GO

SELECT COUNT(*) FROM sys.all_objects WHERE type='V' and name = 'rand_name1';
GO

CREATE VIEW rand_name2 AS select 1;
GO

SELECT COUNT(*) FROM sys.views WHERE name = 'rand_name2';
GO

SELECT COUNT(*) FROM sys.objects WHERE type='V' and name = 'rand_name2';
GO

SELECT COUNT(*) FROM sys.all_objects WHERE type='V' and name = 'rand_name2';
GO

USE db1
GO

#view rand_name2 should not be visible in db1 database.
SELECT COUNT(*) FROM sys.views WHERE name = 'rand_name2';
GO

SELECT COUNT(*) FROM sys.objects WHERE type='V' and name = 'rand_name2';
GO

SELECT COUNT(*) FROM sys.all_objects WHERE type='V' and name = 'rand_name2';
GO

DROP VIEW rand_name1;
GO

USE master;
GO

DROP DATABASE db1;
GO

DROP VIEW rand_name2;
GO
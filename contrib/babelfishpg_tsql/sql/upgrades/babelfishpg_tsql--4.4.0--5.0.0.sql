-- complain if script is sourced in psql, rather than via ALTER EXTENSION
\echo Use "ALTER EXTENSION ""babelfishpg_tsql"" UPDATE TO '5.0.0'" to load this file. \quit

-- add 'sys' to search path for the convenience
SELECT set_config('search_path', 'sys, '||current_setting('search_path'), false);

-- Drops an object if it does not have any dependent objects.
-- Is a temporary procedure for use by the upgrade script. Will be dropped at the end of the upgrade.
-- Please have this be one of the first statements executed in this upgrade script. 
CREATE OR REPLACE PROCEDURE babelfish_drop_deprecated_object(object_type varchar, schema_name varchar, object_name varchar) AS
$$
DECLARE
    error_msg text;
    query1 text;
    query2 text;
BEGIN

    query1 := pg_catalog.format('alter extension babelfishpg_tsql drop %s %s.%s', object_type, schema_name, object_name);
    query2 := pg_catalog.format('drop %s %s.%s', object_type, schema_name, object_name);

    execute query1;
    execute query2;
EXCEPTION
    when object_not_in_prerequisite_state then --if 'alter extension' statement fails
        GET STACKED DIAGNOSTICS error_msg = MESSAGE_TEXT;
        raise warning '%', error_msg;
    when dependent_objects_still_exist then --if 'drop view' statement fails
        GET STACKED DIAGNOSTICS error_msg = MESSAGE_TEXT;
        raise warning '%', error_msg;
end
$$
LANGUAGE plpgsql;

-- please add your SQL here
/*
 * Note: These SQL statements may get executed multiple times specially when some features get backpatched.
 * So make sure that any SQL statement (DDL/DML) being added here can be executed multiple times without affecting
 * final behaviour.
 */

CREATE OR REPLACE VIEW information_schema_tsql.table_constraints AS
    SELECT CAST(nc.dbname AS sys.nvarchar(128)) AS "CONSTRAINT_CATALOG",
           CAST(extc.orig_name AS sys.nvarchar(128)) AS "CONSTRAINT_SCHEMA",
           CAST(c.conname AS sys.sysname) AS "CONSTRAINT_NAME",
           CAST(nr.dbname AS sys.nvarchar(128)) AS "TABLE_CATALOG",
           CAST(extr.orig_name AS sys.nvarchar(128)) AS "TABLE_SCHEMA",
           CAST(r.relname AS sys.sysname) AS "TABLE_NAME",
           CAST(
             CASE c.contype WHEN 'c' THEN 'CHECK'
                            WHEN 'f' THEN 'FOREIGN KEY'
                            WHEN 'p' THEN 'PRIMARY KEY'
                            WHEN 'u' THEN 'UNIQUE' END
             AS sys.varchar(11)) COLLATE sys.database_default AS "CONSTRAINT_TYPE",
           CAST('NO' AS sys.varchar(2)) AS "IS_DEFERRABLE",
           CAST('NO' AS sys.varchar(2)) AS "INITIALLY_DEFERRED"

    FROM sys.pg_namespace_ext nc LEFT OUTER JOIN sys.babelfish_namespace_ext extc ON nc.nspname = extc.nspname,
         sys.pg_namespace_ext nr LEFT OUTER JOIN sys.babelfish_namespace_ext extr ON nr.nspname = extr.nspname,
         pg_constraint c,
         pg_class r

    WHERE nc.oid = c.connamespace AND nr.oid = r.relnamespace
          AND c.conrelid = r.oid
          AND c.contype IN ('c', 'f', 'p', 'u')
          AND r.relkind IN ('r', 'p')
          AND relispartition = false
          AND (NOT pg_is_other_temp_schema(nr.oid))
          AND (pg_has_role(r.relowner, 'USAGE')
               OR has_table_privilege(r.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
               OR has_any_column_privilege(r.oid, 'SELECT, INSERT, UPDATE, REFERENCES') )
		  AND  extc.dbid = sys.db_id();

GRANT SELECT ON information_schema_tsql.table_constraints TO PUBLIC;

CREATE OR REPLACE PROCEDURE sys.create_db_roles_during_upgrade()
LANGUAGE C
AS 'babelfishpg_tsql', 'create_db_roles_during_upgrade';

CALL sys.create_db_roles_during_upgrade();

DROP PROCEDURE sys.create_db_roles_during_upgrade();

DO
LANGUAGE plpgsql
$$
DECLARE securityadmin TEXT;
BEGIN
    IF EXISTS (
               SELECT FROM pg_catalog.pg_roles
               WHERE  rolname = 'securityadmin') 
        THEN
            RAISE EXCEPTION 'Role "securityadmin" already exists.';
    ELSE
        EXECUTE format('CREATE ROLE securityadmin CREATEROLE INHERIT PASSWORD NULL');
        EXECUTE format('GRANT securityadmin TO bbf_role_admin WITH ADMIN TRUE');
        CALL sys.babel_initialize_logins('securityadmin');
    END IF;
END;
$$;

CREATE OR REPLACE FUNCTION sys.bbf_is_member_of_role_nosuper(OID, OID)
RETURNS BOOLEAN AS 'babelfishpg_tsql', 'bbf_is_member_of_role_nosuper'
LANGUAGE C STABLE STRICT PARALLEL SAFE;

-- SERVER_PRINCIPALS
CREATE OR REPLACE VIEW sys.server_principals
AS SELECT
CAST(Ext.orig_loginname AS sys.SYSNAME) AS name,
CAST(Base.oid As INT) AS principal_id,
CAST(CAST(Base.oid as INT) as sys.varbinary(85)) AS sid,
CAST(Ext.type AS CHAR(1)) as type,
CAST(
  CASE
    WHEN Ext.type = 'S' THEN 'SQL_LOGIN'
    WHEN Ext.type = 'R' THEN 'SERVER_ROLE'
    WHEN Ext.type = 'U' THEN 'WINDOWS_LOGIN'
    ELSE NULL
  END
  AS NVARCHAR(60)) AS type_desc,
CAST(Ext.is_disabled AS INT) AS is_disabled,
CAST(Ext.create_date AS SYS.DATETIME) AS create_date,
CAST(Ext.modify_date AS SYS.DATETIME) AS modify_date,
CAST(CASE WHEN Ext.type = 'R' THEN NULL ELSE Ext.default_database_name END AS SYS.SYSNAME) AS default_database_name,
CAST(Ext.default_language_name AS SYS.SYSNAME) AS default_language_name,
CAST(CASE WHEN Ext.type = 'R' THEN NULL ELSE Ext.credential_id END AS INT) AS credential_id,
CAST(CASE WHEN Ext.type = 'R' THEN 1 ELSE Ext.owning_principal_id END AS INT) AS owning_principal_id,
CAST(CASE WHEN Ext.type = 'R' THEN 1 ELSE Ext.is_fixed_role END AS sys.BIT) AS is_fixed_role
FROM pg_catalog.pg_roles AS Base INNER JOIN sys.babelfish_authid_login_ext AS Ext ON Base.rolname = Ext.rolname
WHERE (pg_has_role(suser_id(), 'sysadmin'::TEXT, 'MEMBER') 
  OR pg_has_role(suser_id(), 'securityadmin'::TEXT, 'MEMBER')
  OR Ext.orig_loginname = suser_name()
  OR Ext.orig_loginname = (SELECT pg_get_userbyid(datdba) FROM pg_database WHERE datname = CURRENT_DATABASE()) COLLATE sys.database_default
  OR Ext.type = 'R')
  AND Ext.type != 'Z'
UNION ALL
SELECT
CAST('public' AS SYS.SYSNAME) AS name,
CAST(-1 AS INT) AS principal_id,
CAST(CAST(0 as INT) as sys.varbinary(85)) AS sid,
CAST('R' AS CHAR(1)) as type,
CAST('SERVER_ROLE' AS NVARCHAR(60)) AS type_desc,
CAST(0 AS INT) AS is_disabled,
CAST(NULL AS SYS.DATETIME) AS create_date,
CAST(NULL AS SYS.DATETIME) AS modify_date,
CAST(NULL AS SYS.SYSNAME) AS default_database_name,
CAST(NULL AS SYS.SYSNAME) AS default_language_name,
CAST(NULL AS INT) AS credential_id,
CAST(1 AS INT) AS owning_principal_id,
CAST(0 AS sys.BIT) AS is_fixed_role;

GRANT SELECT ON sys.server_principals TO PUBLIC;

-- login_token
CREATE OR REPLACE VIEW sys.login_token
AS SELECT
CAST(Base.oid As INT) AS principal_id,
CAST(CAST(Base.oid as INT) as sys.varbinary(85)) AS sid,
CAST(Ext.orig_loginname AS sys.nvarchar(128)) AS name,
CAST(CASE
WHEN Ext.type = 'U' THEN 'WINDOWS LOGIN'
ELSE 'SQL LOGIN' END AS SYS.NVARCHAR(128)) AS TYPE,
CAST('GRANT OR DENY' as sys.nvarchar(128)) as usage
FROM pg_catalog.pg_roles AS Base INNER JOIN sys.babelfish_authid_login_ext AS Ext ON Base.rolname = Ext.rolname
WHERE Ext.orig_loginname = sys.suser_name()
AND Ext.type in ('S','U')
UNION ALL
SELECT
CAST(Base.oid As INT) AS principal_id,
CAST(CAST(Base.oid as INT) as sys.varbinary(85)) AS sid,
CAST(Ext.orig_loginname AS sys.nvarchar(128)) AS name,
CAST('SERVER ROLE' AS sys.nvarchar(128)) AS type,
CAST ('GRANT OR DENY' as sys.nvarchar(128)) as usage
FROM pg_catalog.pg_roles AS Base INNER JOIN sys.babelfish_authid_login_ext AS Ext ON Base.rolname = Ext.rolname
WHERE Ext.type = 'R'
AND bbf_is_member_of_role_nosuper(sys.suser_id(), Base.oid);

GRANT SELECT ON sys.login_token TO PUBLIC;

CREATE OR REPLACE FUNCTION is_srvrolemember(role sys.SYSNAME, login sys.SYSNAME DEFAULT suser_name())
RETURNS INTEGER AS
$$
DECLARE has_role BOOLEAN;
DECLARE login_valid BOOLEAN;
BEGIN
	role  := TRIM(trailing from LOWER(role));
	login := TRIM(trailing from LOWER(login));
	
	login_valid = (login = suser_name() COLLATE sys.database_default) OR 
		(EXISTS (SELECT name
	 			FROM sys.server_principals
		 	 	WHERE 
				LOWER(name) = login COLLATE sys.database_default
				AND type = 'S'));
 	
 	IF NOT login_valid THEN
 		RETURN NULL;
    
    ELSIF role = 'public' COLLATE sys.database_default THEN
    	RETURN 1;
	
    ELSIF role = 'sysadmin' COLLATE sys.database_default OR role = 'securityadmin' COLLATE sys.database_default THEN
	  	has_role = (pg_has_role(login::TEXT, role::TEXT, 'MEMBER') OR pg_has_role(login::TEXT, 'sysadmin'::TEXT, 'MEMBER'));
	    IF has_role THEN
			RETURN 1;
		ELSE
			RETURN 0;
		END IF;
	
    ELSIF role COLLATE sys.database_default IN (
            'serveradmin',
            'setupadmin',
            'processadmin',
            'dbcreator',
            'diskadmin',
            'bulkadmin') THEN 
    	RETURN 0;
 	
    ELSE
 		  RETURN NULL;
    END IF;
	
 	EXCEPTION WHEN OTHERS THEN
	 	  RETURN NULL;
END;
$$ LANGUAGE plpgsql STABLE;

-- SYSLOGINS
CREATE OR REPLACE VIEW sys.syslogins
AS SELECT 
Base.sid AS sid,
CAST(9 AS SYS.TINYINT) AS status,
Base.create_date AS createdate,
Base.modify_date AS updatedate,
Base.create_date AS accdate,
CAST(0 AS INT) AS totcpu,
CAST(0 AS INT) AS totio,
CAST(0 AS INT) AS spacelimit,
CAST(0 AS INT) AS timelimit,
CAST(0 AS INT) AS resultlimit,
Base.name AS name,
Base.default_database_name AS dbname,
Base.default_language_name AS default_language_name,
CAST(Base.name AS SYS.NVARCHAR(128)) AS loginname,
CAST(NULL AS SYS.NVARCHAR(128)) AS password,
CAST(0 AS INT) AS denylogin,
CAST(1 AS INT) AS hasaccess,
CAST( 
  CASE 
    WHEN Base.type_desc = 'WINDOWS_LOGIN' OR Base.type_desc = 'WINDOWS_GROUP' THEN 1 
    ELSE 0
  END
AS INT) AS isntname,
CAST(
   CASE 
    WHEN Base.type_desc = 'WINDOWS_GROUP' THEN 1 
    ELSE 0
  END
  AS INT) AS isntgroup,
CAST(
  CASE 
    WHEN Base.type_desc = 'WINDOWS_LOGIN' THEN 1 
    ELSE 0
  END
AS INT) AS isntuser,
CAST(
    CASE
        WHEN is_srvrolemember('sysadmin', Base.name) = 1 THEN 1
        ELSE 0
    END
AS INT) AS sysadmin,
CAST(
    CASE
        WHEN is_srvrolemember('securityadmin', Base.name) = 1 THEN 1
        ELSE 0
    END
AS INT) AS securityadmin,
CAST(0 AS INT) AS serveradmin,
CAST(0 AS INT) AS setupadmin,
CAST(0 AS INT) AS processadmin,
CAST(0 AS INT) AS diskadmin,
CAST(0 AS INT) AS dbcreator,
CAST(0 AS INT) AS bulkadmin
FROM sys.server_principals AS Base
WHERE Base.type in ('S', 'U');

GRANT SELECT ON sys.syslogins TO PUBLIC;

CREATE OR REPLACE FUNCTION sys.is_member(IN role sys.SYSNAME)
RETURNS INT AS
$$
DECLARE
    is_windows_grp boolean := (CHARINDEX('\', role) != 0);
BEGIN
    -- Always return 1 for 'public'
    IF (role = 'public' COLLATE sys.database_default )
    THEN RETURN 1;
    END IF;
    IF EXISTS (SELECT orig_loginname FROM sys.babelfish_authid_login_ext WHERE orig_loginname = role COLLATE sys.database_default AND type != 'S') -- do not consider sql logins
    THEN
        IF ((EXISTS (SELECT name FROM sys.login_token WHERE name = role COLLATE sys.database_default AND type IN ('SERVER ROLE', 'SQL LOGIN'))) OR is_windows_grp) -- do not consider sql logins, server roles
        THEN RETURN NULL; -- Also return NULL if session is not a windows auth session but argument is a windows group
        ELSIF EXISTS (SELECT name FROM sys.login_token WHERE name = role COLLATE sys.database_default AND type NOT IN ('SERVER ROLE', 'SQL LOGIN'))
        THEN RETURN 1; -- Return 1 if current session user is a member of role or windows group
        ELSE RETURN 0; -- Return 0 if current session user is not a member of role or windows group
        END IF;
    ELSIF EXISTS (SELECT orig_username FROM sys.babelfish_authid_user_ext WHERE orig_username = role COLLATE sys.database_default)
    THEN
        IF (((SELECT orig_username FROM sys.babelfish_authid_user_ext WHERE rolname = CURRENT_USER) = 'dbo' COLLATE sys.database_default) AND role COLLATE sys.database_default IN ('db_owner', 'db_accessadmin'))
        THEN RETURN 1;
        ELSIF EXISTS (SELECT name FROM sys.user_token WHERE name = role COLLATE sys.database_default)
        THEN RETURN 1; -- Return 1 if current session user is a member of role or windows group
        ELSIF (is_windows_grp)
        THEN RETURN NULL; -- Return NULL if session is not a windows auth session but argument is a windows group
        ELSE RETURN 0; -- Return 0 if current session user is not a member of role or windows group
        END IF;
    ELSE RETURN NULL; -- Return NULL if role/group does not exist
    END IF;
END;
$$
LANGUAGE plpgsql STRICT STABLE;

-- DATABASE_PRINCIPALS
CREATE OR REPLACE VIEW sys.database_principals AS
SELECT
CAST(Ext.orig_username AS SYS.SYSNAME) AS name,
CAST(Base.oid AS INT) AS principal_id,
CAST(Ext.type AS CHAR(1)) as type,
CAST(
  CASE
    WHEN Ext.type = 'S' THEN 'SQL_USER'
    WHEN Ext.type = 'R' THEN 'DATABASE_ROLE'
    WHEN Ext.type = 'U' THEN 'WINDOWS_USER'
    ELSE NULL
  END
  AS SYS.NVARCHAR(60)) AS type_desc,
CAST(Ext.default_schema_name AS SYS.SYSNAME) AS default_schema_name,
CAST(Ext.create_date AS SYS.DATETIME) AS create_date,
CAST(Ext.modify_date AS SYS.DATETIME) AS modify_date,
CAST(Ext.owning_principal_id AS INT) AS owning_principal_id,
CAST(CAST(Base2.oid AS INT) AS SYS.VARBINARY(85)) AS SID,
CAST(Ext.is_fixed_role AS SYS.BIT) AS is_fixed_role,
CAST(Ext.authentication_type AS INT) AS authentication_type,
CAST(Ext.authentication_type_desc AS SYS.NVARCHAR(60)) AS authentication_type_desc,
CAST(Ext.default_language_name AS SYS.SYSNAME) AS default_language_name,
CAST(Ext.default_language_lcid AS INT) AS default_language_lcid,
CAST(Ext.allow_encrypted_value_modifications AS SYS.BIT) AS allow_encrypted_value_modifications
FROM pg_catalog.pg_roles AS Base INNER JOIN sys.babelfish_authid_user_ext AS Ext
ON Base.rolname = Ext.rolname
LEFT OUTER JOIN pg_catalog.pg_roles Base2
ON Ext.login_name = Base2.rolname
WHERE Ext.database_name = DB_NAME()
  AND (Ext.orig_username IN ('dbo', 'db_owner', 'db_accessadmin', 'guest') -- system users should always be visible
  OR pg_has_role(Ext.rolname, 'MEMBER')) -- Current user should be able to see users it has permission of
UNION ALL
SELECT
CAST(name AS SYS.SYSNAME) AS name,
CAST(-1 AS INT) AS principal_id,
CAST(type AS CHAR(1)) as type,
CAST(
  CASE
    WHEN type = 'S' THEN 'SQL_USER'
    WHEN type = 'R' THEN 'DATABASE_ROLE'
    WHEN type = 'U' THEN 'WINDOWS_USER'
    ELSE NULL
  END
  AS SYS.NVARCHAR(60)) AS type_desc,
CAST(NULL AS SYS.SYSNAME) AS default_schema_name,
CAST(NULL AS SYS.DATETIME) AS create_date,
CAST(NULL AS SYS.DATETIME) AS modify_date,
CAST(-1 AS INT) AS owning_principal_id,
CAST(CAST(0 AS INT) AS SYS.VARBINARY(85)) AS SID,
CAST(0 AS SYS.BIT) AS is_fixed_role,
CAST(-1 AS INT) AS authentication_type,
CAST(NULL AS SYS.NVARCHAR(60)) AS authentication_type_desc,
CAST(NULL AS SYS.SYSNAME) AS default_language_name,
CAST(-1 AS INT) AS default_language_lcid,
CAST(0 AS SYS.BIT) AS allow_encrypted_value_modifications
FROM (VALUES ('public', 'R'), ('sys', 'S'), ('INFORMATION_SCHEMA', 'S')) as dummy_principals(name, type);
GRANT SELECT ON sys.database_principals TO PUBLIC;

CREATE OR REPLACE PROCEDURE sys.sp_helpdbfixedrole("@rolename" sys.SYSNAME = NULL) AS
$$
BEGIN
	-- Returns a list of the fixed database roles. 
	IF LOWER(RTRIM(@rolename)) IS NULL OR LOWER(RTRIM(@rolename)) IN ('db_owner', 'db_accessadmin')
	BEGIN
		SELECT CAST(DbFixedRole as sys.SYSNAME) AS DbFixedRole, CAST(Description AS sys.nvarchar(70)) AS Description FROM (
			VALUES ('db_owner', 'DB Owners'),
			('db_accessadmin', 'DB Access Administrators')) x(DbFixedRole, Description)
			WHERE LOWER(RTRIM(@rolename)) IS NULL OR LOWER(RTRIM(@rolename)) = DbFixedRole;
	END
	ELSE IF LOWER(RTRIM(@rolename)) IN (
			'db_securityadmin','db_ddladmin', 'db_backupoperator', 
			'db_datareader', 'db_datawriter', 'db_denydatareader', 'db_denydatawriter')
	BEGIN
		-- Return an empty result set instead of raising an error
		SELECT CAST(NULL AS sys.SYSNAME) AS DbFixedRole, CAST(NULL AS sys.nvarchar(70)) AS Description
		WHERE 1=0;	
	END
	ELSE
		RAISERROR('''%s'' is not a known fixed role.', 16, 1, @rolename);
END
$$
LANGUAGE 'pltsql';
GRANT EXECUTE ON PROCEDURE sys.sp_helpdbfixedrole TO PUBLIC;

CREATE OR REPLACE PROCEDURE sys.sp_helpuser("@name_in_db" sys.SYSNAME = NULL) AS
$$
BEGIN
	-- If security account is not specified, return info about all users
	IF @name_in_db IS NULL
	BEGIN
		SELECT CAST(Ext1.orig_username AS SYS.SYSNAME) AS 'UserName',
			   CAST(CASE WHEN Ext1.orig_username = 'dbo' THEN 'db_owner' 
					WHEN Ext2.orig_username IS NULL THEN 'public'
					ELSE Ext2.orig_username END 
					AS SYS.SYSNAME) AS 'RoleName',
			   CAST(CASE WHEN Ext1.orig_username = 'dbo' THEN Base4.rolname COLLATE database_default
					ELSE LogExt.orig_loginname END
					AS SYS.SYSNAME) AS 'LoginName',
			   CAST(LogExt.default_database_name AS SYS.SYSNAME) AS 'DefDBName',
			   CAST(Ext1.default_schema_name AS SYS.SYSNAME) AS 'DefSchemaName',
			   CAST(Base1.oid AS INT) AS 'UserID',
			   CAST(CASE WHEN Ext1.orig_username = 'dbo' THEN CAST(Base4.oid AS INT)
					WHEN Ext1.orig_username = 'guest' THEN CAST(0 AS INT)
					ELSE CAST(Base3.oid AS INT) END
					AS SYS.VARBINARY(85)) AS 'SID'
		FROM sys.babelfish_authid_user_ext AS Ext1
		INNER JOIN pg_catalog.pg_roles AS Base1 ON Base1.rolname = Ext1.rolname
		LEFT OUTER JOIN pg_catalog.pg_auth_members AS Authmbr ON Base1.oid = Authmbr.member
		LEFT OUTER JOIN pg_catalog.pg_roles AS Base2 ON Base2.oid = Authmbr.roleid
		LEFT OUTER JOIN sys.babelfish_authid_user_ext AS Ext2 ON Base2.rolname = Ext2.rolname
		LEFT OUTER JOIN sys.babelfish_authid_login_ext As LogExt ON LogExt.rolname = Ext1.login_name
		LEFT OUTER JOIN pg_catalog.pg_roles AS Base3 ON Base3.rolname = LogExt.rolname
		LEFT OUTER JOIN sys.babelfish_sysdatabases AS Bsdb ON Bsdb.name = DB_NAME()
		LEFT OUTER JOIN pg_catalog.pg_roles AS Base4 ON Base4.rolname = Bsdb.owner
		WHERE Ext1.database_name = DB_NAME()
		AND (Ext1.type != 'R' OR Ext1.type != 'A')
		AND Ext1.orig_username != 'db_owner'
		AND Ext1.orig_username NOT IN ('db_owner', 'db_accessadmin')
		ORDER BY UserName, RoleName;
	END
	-- If the security account is the db fixed role - db_owner
    ELSE IF @name_in_db = 'db_owner'
	BEGIN
		-- TODO: Need to change after we can add/drop members to/from db_owner
		SELECT CAST('db_owner' AS SYS.SYSNAME) AS 'Role_name',
			   ROLE_ID('db_owner') AS 'Role_id',
			   CAST('dbo' AS SYS.SYSNAME) AS 'Users_in_role',
			   USER_ID('dbo') AS 'Userid';
	END
	-- If the security account is a db role
	ELSE IF EXISTS (SELECT 1
					FROM sys.babelfish_authid_user_ext
					WHERE (orig_username = @name_in_db
					OR pg_catalog.lower(orig_username) = pg_catalog.lower(@name_in_db))
					AND database_name = DB_NAME()
					AND type = 'R')
	BEGIN
		SELECT CAST(Ext1.orig_username AS SYS.SYSNAME) AS 'Role_name',
			   CAST(Base1.oid AS INT) AS 'Role_id',
			   CAST(Ext2.orig_username AS SYS.SYSNAME) AS 'Users_in_role',
			   CAST(Base2.oid AS INT) AS 'Userid'
		FROM sys.babelfish_authid_user_ext AS Ext2
		INNER JOIN pg_catalog.pg_roles AS Base2 ON Base2.rolname = Ext2.rolname
		INNER JOIN pg_catalog.pg_auth_members AS Authmbr ON Base2.oid = Authmbr.member
		LEFT OUTER JOIN pg_catalog.pg_roles AS Base1 ON Base1.oid = Authmbr.roleid
		LEFT OUTER JOIN sys.babelfish_authid_user_ext AS Ext1 ON Base1.rolname = Ext1.rolname
		WHERE Ext1.database_name = DB_NAME()
		AND Ext2.database_name = DB_NAME()
		AND Ext1.type = 'R'
		AND Ext2.orig_username != 'db_owner'
		AND Ext2.orig_username NOT IN ('db_owner', 'db_accessadmin')
		AND (Ext1.orig_username = @name_in_db OR pg_catalog.lower(Ext1.orig_username) = pg_catalog.lower(@name_in_db))
		ORDER BY Role_name, Users_in_role;
	END
	-- If the security account is a user
	ELSE IF EXISTS (SELECT 1
					FROM sys.babelfish_authid_user_ext
					WHERE (orig_username = @name_in_db
					OR pg_catalog.lower(orig_username) = pg_catalog.lower(@name_in_db))
					AND database_name = DB_NAME()
					AND type != 'R')
	BEGIN
		SELECT DISTINCT CAST(Ext1.orig_username AS SYS.SYSNAME) AS 'UserName',
			   CAST(CASE WHEN Ext1.orig_username = 'dbo' THEN 'db_owner' 
					WHEN Ext2.orig_username IS NULL THEN 'public' 
					ELSE Ext2.orig_username END 
					AS SYS.SYSNAME) AS 'RoleName',
			   CAST(CASE WHEN Ext1.orig_username = 'dbo' THEN Base4.rolname COLLATE database_default
					ELSE LogExt.orig_loginname END
					AS SYS.SYSNAME) AS 'LoginName',
			   CAST(LogExt.default_database_name AS SYS.SYSNAME) AS 'DefDBName',
			   CAST(Ext1.default_schema_name AS SYS.SYSNAME) AS 'DefSchemaName',
			   CAST(Base1.oid AS INT) AS 'UserID',
			   CAST(CASE WHEN Ext1.orig_username = 'dbo' THEN CAST(Base4.oid AS INT)
					WHEN Ext1.orig_username = 'guest' THEN CAST(0 AS INT)
					ELSE CAST(Base3.oid AS INT) END
					AS SYS.VARBINARY(85)) AS 'SID'
		FROM sys.babelfish_authid_user_ext AS Ext1
		INNER JOIN pg_catalog.pg_roles AS Base1 ON Base1.rolname = Ext1.rolname
		LEFT OUTER JOIN pg_catalog.pg_auth_members AS Authmbr ON Base1.oid = Authmbr.member
		LEFT OUTER JOIN pg_catalog.pg_roles AS Base2 ON Base2.oid = Authmbr.roleid
		LEFT OUTER JOIN sys.babelfish_authid_user_ext AS Ext2 ON Base2.rolname = Ext2.rolname
		LEFT OUTER JOIN sys.babelfish_authid_login_ext As LogExt ON LogExt.rolname = Ext1.login_name
		LEFT OUTER JOIN pg_catalog.pg_roles AS Base3 ON Base3.rolname = LogExt.rolname
		LEFT OUTER JOIN sys.babelfish_sysdatabases AS Bsdb ON Bsdb.name = DB_NAME()
		LEFT OUTER JOIN pg_catalog.pg_roles AS Base4 ON Base4.rolname = Bsdb.owner
		WHERE Ext1.database_name = DB_NAME()
		AND (Ext1.type != 'R' OR Ext1.type != 'A')
		AND Ext1.orig_username != 'db_owner'
		AND Ext1.orig_username NOT IN ('db_owner', 'db_accessadmin')
		AND (Ext1.orig_username = @name_in_db OR pg_catalog.lower(Ext1.orig_username) = pg_catalog.lower(@name_in_db))
		ORDER BY UserName, RoleName;
	END
	-- If the security account is not valid
	ELSE 
		RAISERROR ( 'The name supplied (%s) is not a user, role, or aliased login.', 16, 1, @name_in_db);
END;
$$
LANGUAGE 'pltsql';
GRANT EXECUTE on PROCEDURE sys.sp_helpuser TO PUBLIC;

-- Drops the temporary procedure used by the upgrade script.
-- Please have this be one of the last statements executed in this upgrade script.
DROP PROCEDURE sys.babelfish_drop_deprecated_object(varchar, varchar, varchar);

-- After upgrade, always run analyze for all babelfish catalogs.
CALL sys.analyze_babelfish_catalogs();

-- Reset search_path to not affect any subsequent scripts
SELECT set_config('search_path', trim(leading 'sys, ' from current_setting('search_path')), false);

CREATE PROCEDURE babel_5186_try_catch_relation_err_proc1
AS
    SELECT * FROM non_existent_table;
GO

CREATE PROCEDURE babel_5186_try_catch_relation_err_proc2
AS
BEGIN
    BEGIN TRAN
        SELECT * FROM non_existent_table;
    COMMIT TRAN
END
GO

CREATE PROCEDURE babel_5186_try_catch_relation_err_proc3
AS
BEGIN
    BEGIN TRY
        EXEC babel_5186_try_catch_relation_err_proc1;
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
					+ ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
					+ ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
					+ ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
					+ ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
					+ ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH;
END
GO

CREATE PROCEDURE babel_5186_try_catch_relation_err_proc4
AS
BEGIN
    EXEC babel_5186_try_catch_relation_err_proc3;
END
GO

-- Triggers with try catch block on same level as trigger body statements
CREATE TABLE babel_5186_table_relation_errTrig1 (a int)
GO

INSERT INTO babel_5186_table_relation_errTrig1 VALUES(1);
INSERT INTO babel_5186_table_relation_errTrig1 VALUES(2);
INSERT INTO babel_5186_table_relation_errTrig1 VALUES(3);
GO

CREATE TRIGGER babel_5186_try_catch_relation_err_trig1_1
ON babel_5186_table_relation_errTrig1
AFTER INSERT
AS
BEGIN
    BEGIN TRY
        SELECT * FROM non_existent_table;
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
                    + ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
                    + ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
                    + ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
                    + ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
                    + ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH
END;
GO

CREATE TRIGGER babel_5186_try_catch_relation_err_trig1_2
ON babel_5186_table_relation_errTrig1
AFTER UPDATE
AS
BEGIN
    BEGIN TRY
        EXEC('SELECT * FROM non_existent_table;');
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
                    + ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
                    + ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
                    + ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
                    + ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
                    + ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH
END;
GO

-- Triggers with try catch block on lower level than trigger body statements
CREATE TABLE babel_5186_table_relation_errTrig2 (a int)
GO

INSERT INTO babel_5186_table_relation_errTrig2 VALUES(1);
INSERT INTO babel_5186_table_relation_errTrig2 VALUES(2);
INSERT INTO babel_5186_table_relation_errTrig2 VALUES(3);
GO

CREATE PROCEDURE babel_5186_proc_relation_errTrig2_1
AS
BEGIN
    BEGIN TRY
        SELECT * FROM non_existent_table;
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
                    + ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
                    + ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
                    + ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
                    + ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
                    + ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH
END
GO

CREATE PROCEDURE babel_5186_proc_relation_errTrig2_2
AS
BEGIN
    BEGIN TRY
        EXEC('SELECT * FROM non_existent_table;');
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
                    + ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
                    + ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
                    + ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
                    + ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
                    + ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH
END
GO

CREATE TRIGGER babel_5186_try_catch_relation_err_trig2_1
ON babel_5186_table_relation_errTrig2
AFTER INSERT
AS
BEGIN
    EXEC babel_5186_proc_relation_errTrig2_1;
END;
GO

CREATE TRIGGER babel_5186_try_catch_relation_err_trig2_2
ON babel_5186_table_relation_errTrig2
AFTER UPDATE
AS
BEGIN
    EXEC babel_5186_proc_relation_errTrig2_2;
END;
GO

-- Triggers with try catch block on higher level than trigger body statements
CREATE TABLE babel_5186_table_relation_errTrig3 (a int)
GO

INSERT INTO babel_5186_table_relation_errTrig3 VALUES(1);
INSERT INTO babel_5186_table_relation_errTrig3 VALUES(2);
INSERT INTO babel_5186_table_relation_errTrig3 VALUES(3);
GO

CREATE TRIGGER babel_5186_try_catch_relation_err_trig3_1
ON babel_5186_table_relation_errTrig3
AFTER INSERT
AS
BEGIN
    SELECT * FROM non_existent_table;
END;
GO

CREATE TRIGGER babel_5186_try_catch_relation_err_trig3_2
ON babel_5186_table_relation_errTrig3
AFTER UPDATE
AS
BEGIN
    EXEC('SELECT * FROM non_existent_table;');
END;
GO

CREATE TABLE babel_5186_try_catch_table (a INT)
GO

CREATE PROCEDURE babel_5186_try_catch_column_err_proc1
AS
    SELECT non_existent_column FROM babel_5186_try_catch_table;
GO

CREATE PROCEDURE babel_5186_try_catch_column_err_proc2
AS
BEGIN
    BEGIN TRAN
        SELECT non_existent_column FROM babel_5186_try_catch_table;
    COMMIT TRAN
END
GO

CREATE PROCEDURE babel_5186_try_catch_column_err_proc3
AS
BEGIN
    BEGIN TRY
        EXEC babel_5186_try_catch_column_err_proc1;
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
					+ ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
					+ ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
					+ ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
					+ ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
					+ ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH;
END
GO

CREATE PROCEDURE babel_5186_try_catch_column_err_proc4
AS
BEGIN
    EXEC babel_5186_try_catch_column_err_proc3;
END
GO

-- Triggers with try catch block on same level as trigger body statements
CREATE TABLE babel_5186_table_column_errTrig1 (a int)
GO

INSERT INTO babel_5186_table_column_errTrig1 VALUES(1);
INSERT INTO babel_5186_table_column_errTrig1 VALUES(2);
INSERT INTO babel_5186_table_column_errTrig1 VALUES(3);
GO

CREATE TRIGGER babel_5186_try_catch_column_err_trig1_1
ON babel_5186_table_column_errTrig1
AFTER INSERT
AS
BEGIN
    BEGIN TRY
        SELECT non_existent_column FROM babel_5186_try_catch_table;
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
                    + ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
                    + ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
                    + ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
                    + ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
                    + ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH
END;
GO

CREATE TRIGGER babel_5186_try_catch_column_err_trig1_2
ON babel_5186_table_column_errTrig1
AFTER UPDATE
AS
BEGIN
    BEGIN TRY
        EXEC('SELECT non_existent_column FROM babel_5186_try_catch_table;');
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
                    + ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
                    + ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
                    + ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
                    + ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
                    + ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH
END;
GO

-- Triggers with try catch block on lower level than trigger body statements
CREATE TABLE babel_5186_table_column_errTrig2 (a int)
GO

INSERT INTO babel_5186_table_column_errTrig2 VALUES(1);
INSERT INTO babel_5186_table_column_errTrig2 VALUES(2);
INSERT INTO babel_5186_table_column_errTrig2 VALUES(3);
GO

CREATE PROCEDURE babel_5186_proc_column_errTrig2_1
AS
BEGIN
    BEGIN TRY
        SELECT non_existent_column FROM babel_5186_try_catch_table;
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
                    + ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
                    + ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
                    + ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
                    + ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
                    + ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH
END
GO

CREATE PROCEDURE babel_5186_proc_column_errTrig2_2
AS
BEGIN
    BEGIN TRY
        EXEC('SELECT non_existent_column FROM babel_5186_try_catch_table;');
    END TRY
    BEGIN CATCH
        SELECT 'Severity_' + cast(ERROR_SEVERITY() as nvarchar(500))
                    + ' Error State_'+ cast(ERROR_STATE() as nvarchar(500))
                    + ' Xact State_'+ cast(XACT_STATE() as nvarchar(500))
                    + ' Error number_'+ cast(ERROR_NUMBER() as nvarchar(500))
                    + ' Error Line number_'+ cast(ERROR_LINE() as nvarchar(500))
                    + ' Error message_'+ cast(ERROR_MESSAGE() as nvarchar(500));
    END CATCH
END
GO

CREATE TRIGGER babel_5186_try_catch_column_err_trig2_1
ON babel_5186_table_column_errTrig2
AFTER INSERT
AS
BEGIN
    EXEC babel_5186_proc_column_errTrig2_1;
END;
GO

CREATE TRIGGER babel_5186_try_catch_column_err_trig2_2
ON babel_5186_table_column_errTrig2
AFTER UPDATE
AS
BEGIN
    EXEC babel_5186_proc_column_errTrig2_2;
END;
GO

-- Triggers with try catch block on higher level than trigger body statements
CREATE TABLE babel_5186_table_column_errTrig3 (a int)
GO

INSERT INTO babel_5186_table_column_errTrig3 VALUES(1);
INSERT INTO babel_5186_table_column_errTrig3 VALUES(2);
INSERT INTO babel_5186_table_column_errTrig3 VALUES(3);
GO

CREATE TRIGGER babel_5186_try_catch_column_err_trig3_1
ON babel_5186_table_column_errTrig3
AFTER INSERT
AS
BEGIN
    SELECT non_existent_column FROM babel_5186_try_catch_table;
END;
GO

CREATE TRIGGER babel_5186_try_catch_column_err_trig3_2
ON babel_5186_table_column_errTrig3
AFTER UPDATE
AS
BEGIN
    EXEC('SELECT non_existent_column FROM babel_5186_try_catch_table;');
END;
GO


CREATE TABLE babel_5186_table_errTable (a int)
GO

-- Simple procedure with transaction
CREATE PROCEDURE babel_5186_errProc1_1
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
BEGIN TRAN;
INSERT INTO babel_5186_table_errTable VALUES (2);
SELECT * FROM non_existent_table;
COMMIT TRAN;
INSERT INTO babel_5186_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc1_2
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
BEGIN TRAN;
INSERT INTO babel_5186_table_errTable VALUES (2);
EXEC('SELECT * FROM non_existent_table;');
COMMIT TRAN;
INSERT INTO babel_5186_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc1_3
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
BEGIN TRAN;
INSERT INTO babel_5186_table_errTable VALUES (2);
SELECT non_existent_column FROM babel_5186_try_catch_table;
COMMIT TRAN;
INSERT INTO babel_5186_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc1_4
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
BEGIN TRAN;
INSERT INTO babel_5186_table_errTable VALUES (2);
EXEC('SELECT non_existent_column FROM babel_5186_try_catch_table;');
COMMIT TRAN;
INSERT INTO babel_5186_table_errTable VALUES (3);
COMMIT TRAN;
GO

-- Nested procedure
CREATE PROCEDURE babel_5186_errProc2_1
AS
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
INSERT INTO babel_5186_table_errTable VALUES (2);
SELECT * FROM non_existent_table;
INSERT INTO babel_5186_table_errTable VALUES (3);
GO

CREATE PROCEDURE babel_5186_errProc2_11
AS
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (4);
EXEC babel_5186_errProc2_1;
INSERT INTO babel_5186_table_errTable VALUES (5);
GO

CREATE PROCEDURE babel_5186_errProc2_2
AS
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
INSERT INTO babel_5186_table_errTable VALUES (2);
EXEC('SELECT * FROM non_existent_table;');
INSERT INTO babel_5186_table_errTable VALUES (3);
GO

CREATE PROCEDURE babel_5186_errProc2_21
AS
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (4);
EXEC babel_5186_errProc2_2;
INSERT INTO babel_5186_table_errTable VALUES (5);
GO

CREATE PROCEDURE babel_5186_errProc2_3
AS
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
INSERT INTO babel_5186_table_errTable VALUES (2);
SELECT non_existent_column FROM babel_5186_try_catch_table;
INSERT INTO babel_5186_table_errTable VALUES (3);
GO

CREATE PROCEDURE babel_5186_errProc2_31
AS
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (4);
EXEC babel_5186_errProc2_3;
INSERT INTO babel_5186_table_errTable VALUES (5);
GO

CREATE PROCEDURE babel_5186_errProc2_4
AS
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
INSERT INTO babel_5186_table_errTable VALUES (2);
EXEC('SELECT non_existent_column FROM babel_5186_try_catch_table;');
INSERT INTO babel_5186_table_errTable VALUES (3);
GO

CREATE PROCEDURE babel_5186_errProc2_41
AS
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (4);
EXEC babel_5186_errProc2_4;
INSERT INTO babel_5186_table_errTable VALUES (5);
GO

-- Nest procedure with transaction
CREATE PROCEDURE babel_5186_errProc3_1
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
INSERT INTO babel_5186_table_errTable VALUES (2);
SELECT * FROM non_existent_table;
INSERT INTO babel_5186_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc3_11
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (4);
EXEC babel_5186_errProc3_1;
INSERT INTO babel_5186_table_errTable VALUES (5);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc3_2
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
INSERT INTO babel_5186_table_errTable VALUES (2);
EXEC('SELECT * FROM non_existent_table;');
INSERT INTO babel_5186_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc3_21
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (4);
EXEC babel_5186_errProc3_2;
INSERT INTO babel_5186_table_errTable VALUES (5);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc3_3
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
INSERT INTO babel_5186_table_errTable VALUES (2);
SELECT non_existent_column FROM babel_5186_try_catch_table;
INSERT INTO babel_5186_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc3_31
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (4);
EXEC babel_5186_errProc3_3;
INSERT INTO babel_5186_table_errTable VALUES (5);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc3_4
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (1);
INSERT INTO babel_5186_table_errTable VALUES (2);
EXEC('SELECT non_existent_column FROM babel_5186_try_catch_table;');
INSERT INTO babel_5186_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_errProc3_41
AS
BEGIN TRAN
SET XACT_ABORT ON;
INSERT INTO babel_5186_table_errTable VALUES (4);
EXEC babel_5186_errProc3_4;
INSERT INTO babel_5186_table_errTable VALUES (5);
COMMIT TRAN;
GO


-- XACT_ABORT OFF
CREATE TABLE babel_5186_1_try_catch_table (a INT)
GO

CREATE TABLE babel_5186_1_table_errTable (a int)
GO
-- Simple procedure with transaction
CREATE PROCEDURE babel_5186_1_errProc1_1
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
BEGIN TRAN;
INSERT INTO babel_5186_1_table_errTable VALUES (2);
SELECT * FROM non_existent_table;
COMMIT TRAN;
INSERT INTO babel_5186_1_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc1_2
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
BEGIN TRAN;
INSERT INTO babel_5186_1_table_errTable VALUES (2);
EXEC('SELECT * FROM non_existent_table;');
COMMIT TRAN;
INSERT INTO babel_5186_1_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc1_3
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
BEGIN TRAN;
INSERT INTO babel_5186_1_table_errTable VALUES (2);
SELECT non_existent_column FROM babel_5186_1_try_catch_table;
COMMIT TRAN;
INSERT INTO babel_5186_1_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc1_4
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
BEGIN TRAN;
INSERT INTO babel_5186_1_table_errTable VALUES (2);
EXEC('SELECT non_existent_column FROM babel_5186_1_try_catch_table;');
COMMIT TRAN;
INSERT INTO babel_5186_1_table_errTable VALUES (3);
COMMIT TRAN;
GO

-- Nested procedure
CREATE PROCEDURE babel_5186_1_errProc2_1
AS
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
INSERT INTO babel_5186_1_table_errTable VALUES (2);
SELECT * FROM non_existent_table;
INSERT INTO babel_5186_1_table_errTable VALUES (3);
GO

CREATE PROCEDURE babel_5186_1_errProc2_11
AS
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (4);
EXEC babel_5186_1_errProc2_1;
INSERT INTO babel_5186_1_table_errTable VALUES (5);
GO

CREATE PROCEDURE babel_5186_1_errProc2_2
AS
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
INSERT INTO babel_5186_1_table_errTable VALUES (2);
EXEC('SELECT * FROM non_existent_table;');
INSERT INTO babel_5186_1_table_errTable VALUES (3);
GO

CREATE PROCEDURE babel_5186_1_errProc2_21
AS
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (4);
EXEC babel_5186_1_errProc2_2;
INSERT INTO babel_5186_1_table_errTable VALUES (5);
GO

CREATE PROCEDURE babel_5186_1_errProc2_3
AS
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
INSERT INTO babel_5186_1_table_errTable VALUES (2);
SELECT non_existent_column FROM babel_5186_1_try_catch_table;
INSERT INTO babel_5186_1_table_errTable VALUES (3);
GO

CREATE PROCEDURE babel_5186_1_errProc2_31
AS
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (4);
EXEC babel_5186_1_errProc2_3;
INSERT INTO babel_5186_1_table_errTable VALUES (5);
GO

CREATE PROCEDURE babel_5186_1_errProc2_4
AS
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
INSERT INTO babel_5186_1_table_errTable VALUES (2);
EXEC('SELECT non_existent_column FROM babel_5186_1_try_catch_table;');
INSERT INTO babel_5186_1_table_errTable VALUES (3);
GO

CREATE PROCEDURE babel_5186_1_errProc2_41
AS
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (4);
EXEC babel_5186_1_errProc2_4;
INSERT INTO babel_5186_1_table_errTable VALUES (5);
GO

-- Nest procedure with transaction
CREATE PROCEDURE babel_5186_1_errProc3_1
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
INSERT INTO babel_5186_1_table_errTable VALUES (2);
SELECT * FROM non_existent_table;
INSERT INTO babel_5186_1_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc3_11
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (4);
EXEC babel_5186_1_errProc3_1;
INSERT INTO babel_5186_1_table_errTable VALUES (5);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc3_2
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
INSERT INTO babel_5186_1_table_errTable VALUES (2);
EXEC('SELECT * FROM non_existent_table;');
INSERT INTO babel_5186_1_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc3_21
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (4);
EXEC babel_5186_1_errProc3_2;
INSERT INTO babel_5186_1_table_errTable VALUES (5);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc3_3
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
INSERT INTO babel_5186_1_table_errTable VALUES (2);
SELECT non_existent_column FROM babel_5186_1_try_catch_table;
INSERT INTO babel_5186_1_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc3_31
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (4);
EXEC babel_5186_1_errProc3_3;
INSERT INTO babel_5186_1_table_errTable VALUES (5);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc3_4
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (1);
INSERT INTO babel_5186_1_table_errTable VALUES (2);
EXEC('SELECT non_existent_column FROM babel_5186_1_try_catch_table;');
INSERT INTO babel_5186_1_table_errTable VALUES (3);
COMMIT TRAN;
GO

CREATE PROCEDURE babel_5186_1_errProc3_41
AS
BEGIN TRAN
SET XACT_ABORT OFF;
INSERT INTO babel_5186_1_table_errTable VALUES (4);
EXEC babel_5186_1_errProc3_4;
INSERT INTO babel_5186_1_table_errTable VALUES (5);
COMMIT TRAN;
GO

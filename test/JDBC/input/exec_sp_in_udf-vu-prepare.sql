use master
go
create table t1_exec_sp_in_udf (a int)
go
create procedure sp_myproc as return 0
go
create function f_scalar_exec_sp_in_udf(@p int) 
returns int as
begin
return @p
end
go

create function f_tvf_exec_sp_in_udf() 
returns @tv table (a int)
begin
	insert @tv values(123)
	return
end
go

create function f_itvf_exec_sp_in_udf() 
returns table 
	return (select 123 as a)
go

use tempdb
go
create procedure p_tempdb_exec_sp_in_udf as return 0
go
use master
go

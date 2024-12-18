use master
go

create function f1_exec_sp_in_udf() 
returns int as
begin
exec sp_executesql 'select 123'
return 0
end
go
select dbo.f1_exec_sp_in_udf()
go
declare @v int = dbo.f1_exec_sp_in_udf()
go
declare @v int 
select @v = dbo.f1_exec_sp_in_udf()
go
declare @v int
set @v = dbo.f1_exec_sp_in_udf()
go
select * from t1_exec_sp_in_udf where a = dbo.f1_exec_sp_in_udf()
go
insert t1_exec_sp_in_udf values(dbo.f1_exec_sp_in_udf())
go
delete t1_exec_sp_in_udf where a = dbo.f1_exec_sp_in_udf()
go
declare @v int
set @v = (select dbo.f1_exec_sp_in_udf())
go
select sqrt(dbo.f1_exec_sp_in_udf())
go
create table t2_exec_sp_in_udf(a int, b int default dbo.f1_exec_sp_in_udf())
go
insert t2_exec_sp_in_udf(a) values(1)
go

create function f2_exec_sp_in_udf() 
returns int as
begin
exec dbo.sp_helpdb
return 0
end
go
select dbo.f2_exec_sp_in_udf()
go

create function f3_exec_sp_in_udf() 
returns int as
begin
exec [sp_helpdb]
return 0
end
go
select dbo.f3_exec_sp_in_udf()
go

create function f4_exec_sp_in_udf() 
returns @tv table(a int) as
begin
exec sp_myproc
return
end
go
select * from dbo.f4_exec_sp_in_udf()
go

create function f5_exec_sp_in_udf() 
returns @tv table(a int) as
begin
exec dbo.[sp_myproc]
return
end
go
select * from dbo.f5_exec_sp_in_udf()
go

create function f6_exec_sp_in_udf() 
returns @tv table(a int) as
begin
exec tempdb..p_tempdb_exec_sp_in_udf
return
end
go
select * from dbo.f6_exec_sp_in_udf()
go

-- do not allow calling with EXEC
create function f6a_exec_sp_in_udf(@p int) 
returns int as
begin
declare @v int
exec @v = f1_exec_sp_in_udf
return @v
end
go
select dbo.f6a_exec_sp_in_udf(123)
go

-- multi-statement TVF cannot be called with EXEC
exec dbo.f_tvf_exec_sp_in_udf
go
create function f7_exec_sp_in_udf() 
returns int
begin
exec dbo.f_tvf_exec_sp_in_udf
return
end
go
select * from dbo.f7_exec_sp_in_udf()
go

-- inline TVF should not be callable with EXEC, but Babelfish currently allows this
-- when this gets fixed so that an error is raised, the result of this test will change
exec dbo.f_itvf_exec_sp_in_udf
go
create function f8_exec_sp_in_udf() 
returns int
begin
exec dbo.f_itvf_exec_sp_in_udf
return
end
go
select * from dbo.f8_exec_sp_in_udf()
go


-- procedure and trigger are not affected:
create trigger tr1 on t1_exec_sp_in_udf for insert as
begin
	exec sp_executesql 'select 123 as in_trigger'
end
go
insert t1_exec_sp_in_udf values(456)
go
select * from t1_exec_sp_in_udf
go

create procedure p1_exec_sp_in_udf
as
begin
	exec sp_executesql 'select 123 as in_procedure'
end
go
exec p1_exec_sp_in_udf
go

-- scalar UDF can still be called with EXEC
declare @v int
exec @v = f_scalar_exec_sp_in_udf 123
select @v as f_scalar_exec_sp_in_udf
go

create function f9_exec_sp_in_udf(@p int) 
returns int as
begin
declare @v int
exec @v = f_scalar_exec_sp_in_udf @p
return @v
end
go
select dbo.f9_exec_sp_in_udf(123)
go

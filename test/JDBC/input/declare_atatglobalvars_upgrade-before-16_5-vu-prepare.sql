create table t1_declare_atatglobalvars_upgrade(a int)
go
insert t1_declare_atatglobalvars_upgrade values (1), (2)
go

-- these objects can be created and executed without error in previous Babelfish releases,
-- but they will fail once this fix is active.
create procedure p1_declare_atatglobalvars_upgrade
as
declare @@max_precision int=123
select @@max_precision
go
exec p1_declare_atatglobalvars_upgrade
go

create function f1_declare_atatglobalvars_upgrade() 
returns int 
as 
begin
declare @@max_precision int=123
return @@max_precision
end
go
select dbo.f1_declare_atatglobalvars_upgrade() 
go

create trigger tr1 on t1_declare_atatglobalvars_upgrade for insert
as
begin
declare @@max_precision int
select @@max_precision
end
go
insert into t1_declare_atatglobalvars_upgrade values(3)
go

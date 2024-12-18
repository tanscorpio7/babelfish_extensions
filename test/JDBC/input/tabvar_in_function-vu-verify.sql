create function f1_tabvar_in_function_ins(@p1 int)
returns int
as
begin
    insert t1_tabvar_in_function_ins values(@p1)
    return 0
end
go

create function f1_tabvar_in_function_upd(@p1 int)
returns int
as
begin
    update t1_tabvar_in_function_ins set a = a + 1
    return 0
end
go

create function f1_tabvar_in_function_del(@p1 int)
returns int
as
begin
    delete t1_tabvar_in_function_ins 
    return 0
end
go

create function f2_tabvar_in_function_ins(@p1 int)
returns int
as
begin
	declare @tv table(a int)
    insert @tv values(@p1)
    return (select sum(a) from @tv)
end
go
select dbo. f2_tabvar_in_function_ins(123) 
go

create function f2_tabvar_in_function_upd(@p1 int)
returns int
as
begin
	declare @tv table(a int)
    insert @tv values(@p1)
    update @tv set a = a + 1
    return (select sum(a) from @tv)
end
go
select dbo. f2_tabvar_in_function_upd(123) 
go

create function f2_tabvar_in_function_del(@p1 int)
returns int
as
begin
	declare @tv table(a int)
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    update @tv set a = a + 1
    delete @tv where a = 124
    return (select sum(a) from @tv)
end
go
select dbo. f2_tabvar_in_function_del(123) 
go

create function f3_tabvar_in_function_upd(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update t set a = t.a + 1 from @tv as t
    return
end
go
select * from dbo. f3_tabvar_in_function_upd(123) order by 1
go

create function f3_tabvar_in_function_del(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    delete t from @tv as t where a = 124 
    return
end
go
select * from dbo. f3_tabvar_in_function_del(123) order by 1
go

create function f4_tabvar_in_function_upd(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update t set a = t.a + 1 from t1_tabvar_in_function_ins, @tv as t where t1_tabvar_in_function_ins.a = t.a
    return
end
go
select * from dbo. f4_tabvar_in_function_upd(123) order by 1
go

create function f4_tabvar_in_function_del(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    delete t from t1_tabvar_in_function_ins, @tv as t where t1_tabvar_in_function_ins.a = t.a
    return
end
go
select * from dbo. f4_tabvar_in_function_del(123) order by 1
go

create function f5_tabvar_in_function_upd(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update t set a = t.a + 1 from @tv as tv, t1_tabvar_in_function_ins as t where tv.a = t.a
    return
end
go

create function f5_tabvar_in_function_del(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    delete t from @tv as tv, t1_tabvar_in_function_ins as t where tv.a = t.a
    return
end
go

create function f6_tabvar_in_function_upd(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update t set a = t.a + 1 from @tv as t join t1_tabvar_in_function_ins as tv on tv.a = t.a
    return
end
go
select * from dbo.f6_tabvar_in_function_upd(123)
go

create function f6_tabvar_in_function_del(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    delete t from @tv as t inner join t1_tabvar_in_function_ins as tv on tv.a = t.a
    return
end
go
select * from dbo.f6_tabvar_in_function_del(123)
go

create function f6a_tabvar_in_function_upd(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update t set a = t.a + 1 from t1_tabvar_in_function_ins as tv join @tv as t on tv.a = t.a
    return
end
go
select * from dbo.f6a_tabvar_in_function_upd(123)
go

create function f6a_tabvar_in_function_del(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    delete t from t1_tabvar_in_function_ins as tv join @tv as t on tv.a = t.a
    return
end
go
select * from dbo.f6a_tabvar_in_function_del(123)
go

create function f7_tabvar_in_function_upd(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update t set a = t.a + 1 from t1_tabvar_in_function_ins as t1 left join t1_tabvar_in_function_ins as tv on tv.a = t1.a right join @tv t on tv.a = t.a 
    return
end
go
select * from dbo.f7_tabvar_in_function_upd(123)
go

create function f7_tabvar_in_function_del(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    delete t from t1_tabvar_in_function_ins as t1 join t1_tabvar_in_function_ins as tv on tv.a = t1.a join @tv t on tv.a = t.a 
    return
end
go
select * from dbo.f7_tabvar_in_function_del(123)
go

create function f8_tabvar_in_function_upd(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update t set a = t.a + 1 from @tv as tv cross join t1_tabvar_in_function_ins as t
    return
end
go

create function f8_tabvar_in_function_del(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    delete t from @tv as tv inner join t1_tabvar_in_function_ins as t on tv.a = t.a
    return
end
go

create function f9_tabvar_in_function_upd(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update t set a = t.a + 1 from @tv as t1 full outer join t1_tabvar_in_function_ins as tv on tv.a = t1.a cross join t1_tabvar_in_function_ins t
    return
end
go

create function f9_tabvar_in_function_del(@p1 int)
returns @tv table(a int)
as
begin
    insert @tv values(@p1)
    update @tv set a = a + 1
    insert @tv values(@p1)
    delete t from @tv as t1 right join t1_tabvar_in_function_ins as tv on tv.a = t1.a full outer join t1_tabvar_in_function_ins t on tv.a = t.a 
    return
end
go

EXECUTE sp_babelfish_configure 'escape_hatch_unique_constraint', 'ignore'
go

select a, value from babel_621_vu_prepare_table_8 order by a;
go

drop index idx on babel_621_vu_prepare_table_10;
go
insert into babel_621_vu_prepare_table_10 values(1, 2, 2);
go
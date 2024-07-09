CREATE TABLE babel_4888_t1 (id1 INT UNIQUE(id1 DESC), id2 VARCHAR(30) UNIQUE(id2 ASC), id3 VARBINARY(30))
GO
CREATE TABLE babel_4888_t2 (id1 INT, id2 VARCHAR(30), id3 VARBINARY(30), UNIQUE(id1 ASC, id2 DESC))
GO
CREATE TABLE babel_4888_t3 (id1 INT PRIMARY KEY(id1 DESC), id2 VARCHAR(30) UNIQUE(id2), id3 VARBINARY(30))
GO
CREATE TABLE babel_4888_t4 (id1 INT, id2 VARCHAR(30), id3 VARBINARY(30), PRIMARY KEY(id1 ASC, id2 DESC))
GO
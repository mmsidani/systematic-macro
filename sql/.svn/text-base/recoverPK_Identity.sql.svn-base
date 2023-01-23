-- sequence of statements i had to go through to recover primary keys and identity property on tables obtained through import/export facility in sql server
-- Note: an incompatibility between the date formats in sql 2008 and 2012 forced us to create tables on sol before the import as opposed to having them created as part of the import from terra.
-- Note2: if tables are created ahead of the import, DO NOT set primary keys on them

alter table tbl_rate_header add temp_id int identity(1,1)
update tbl_sa2_macro_data set Rate_Id=(select temp_id from tbl_rate_header where tbl_rate_header.Rate_Id=tbl_sa2_macro_data.Rate_Id)
update tbl_rate_data set Rate_Id=(select temp_id from tbl_rate_header where tbl_rate_header.Rate_Id=tbl_rate_data.Rate_Id)
alter Table tbl_sa2_macro_data ADD PRIMARY KEY (Rate_Id,Date,Date_Entered)
alter Table tbl_rate_data ADD PRIMARY KEY (Rate_Id,Date)
alter Table tbl_rate_header drop column Rate_Id
-- now rename column temp_id Rate_Id by right-clicking in management studio
alter Table tbl_rate_header ADD PRIMARY KEY (Rate_Id)
-- finally create the constraint on tbl_sa2_macro_data by executing CheckSA2MacroTable.sql
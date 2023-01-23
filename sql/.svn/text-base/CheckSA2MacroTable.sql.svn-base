SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Execute this first in Management Studio as new query and then run SetRevisableMacroConstraint.sql
-- =============================================
CREATE function [CheckSA2MacroRevisable] 
(
	-- Add the parameters for the function here
	@Rate_Id int,
	@Label varchar(10),
	@Is_Revisable bit
)
RETURNS int
AS
BEGIN
	-- Declare the return variable here
	DECLARE @ret int;

	-- Add the T-SQL statements to compute the return value here
	SELECT @ret = COUNT(*) FROM tbl_sa2_macro_data WHERE Rate_Id = @Rate_Id AND Label = @Label AND Is_Revisable = @Is_Revisable;

	-- Return the result of the function
	RETURN @ret;

END
GO
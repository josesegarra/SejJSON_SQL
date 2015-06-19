
/****** Object:  UserDefinedTableType [dbo].[pjsonData]    Script Date: 6/16/2015 3:12:22 PM ******/
CREATE TYPE [dbo].[pjsonData] AS TABLE(
	[id] [int] NOT NULL,
	[parent] [int] NOT NULL,
	[name] [nvarchar](100) NOT NULL,
	[kind] [nvarchar](10) NOT NULL,
	[value] [nvarchar](max) NOT NULL
)
GO
/****** Object:  UserDefinedFunction [dbo].[json_Array]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO




/************* json_Array *********************/

CREATE FUNCTION [dbo].[json_Array](@id int,@parent int, @start int,@JSON NVARCHAR(MAX))
RETURNS @hierarchy table
(
  id int,parent int,name nvarchar(2000),kind nvarchar(10),ppos int,pend int,value nvarchar(MAX) NOT NULL
)
AS
	BEGIN
		declare @pos int,@m int,@b int=@start
        -- Look for first item after [
		set @pos=dbo.json_Skip(@json,@start)
		if (@pos = 0) begin
			if (right(@json,1)=']') return -- If found because this is an empty array then do not raise an error
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,'','ERROR',@start,-1,'Wrong array definition')
			return
			end		
		-- If is ] then we have an empty array, lets return
		if (substring(@json,@pos,1)=']') return

		-- Enter endless loop
		while (1=1) begin
			-- Insert item into hierarchy
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) select id,parent,name,kind,ppos,pend,value from [dbo].[json_Item](@id,@parent,@pos,@json,0)
            

			-- If nothing was inserted then return	
			if not exists(select * from @hierarchy) begin
					insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(0,0,'','ERROR',@pos,-1,'Unexpected error')
					break
					end
			-- If an error happened then return
			if exists(select * from @hierarchy where kind='ERROR') break
			-- Get MAX id of inserted objects and ADD 1. This sets the new ID
			select @id=max(id)+1 from @hierarchy 
			-- Get latest position of readed object
			select @m=max(pend) from @hierarchy 
            -- Skip after 
			set @pos = dbo.json_Skip(@json,@m)
            -- If we do not have a [,] then exit loop
			if (substring(@json,@pos,1)!=',') break
            -- Move after ,
			set @pos = dbo.json_Skip(@json,@pos+1)
			end
	return
	end


GO
/****** Object:  UserDefinedFunction [dbo].[json_Item]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



/****** json_Item  ******/
CREATE FUNCTION [dbo].[json_Item](@id int,@parent int, @start int,@JSON NVARCHAR(MAX),@parseName int)
RETURNS @hierarchy table(id int,parent int,name nvarchar(2000),kind nvarchar(10),ppos int,pend int,value nvarchar(MAX) NOT NULL)
AS
	BEGIN
		declare @kind   nvarchar(10)
		declare @name   nvarchar(MAX)=''
		declare @value  nvarchar(MAX)
		declare @p		int
		
		if (@parseName=1) begin
			select  @kind=kind,@name=name,@start=pend from dbo.json_Name(@start,@json)
			if (@kind='ERROR') begin
				insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(0,@parent,'','ERROR',@start,-1,@name)
				return
			end
		end 
		set @kind=substring(@json,@start,1)
		set @start=dbo.json_Skip(@json,@start)
		-- Handle strings

		if (@kind='"') begin
			select @value=value,@p=p2 from dbo.json_String(@json,@start+1)
			if (@p=-1) begin
				insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,'','ERROR',@p,-1,isnull(@value,'Bad string'))
				return;																	
			end
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,@name,'STRING',@start,@p,@value)
			return
		end

		-- Handle Objects

		if (@kind='{') begin
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,@name,'OBJECT',@start,@start+1,'')
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) select id,parent,name,kind,ppos,pend,value from dbo.json_Object(@id+1,@id,@start+1,@json)
			if exists(select * from @hierarchy where kind='ERROR') return
			
			select @p=max(pend) from @hierarchy
			set @start = dbo.json_SkipUntil(@json,@p,'}')
			if (@start=0) begin
				insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,'','ERROR',@p,@p,'Expected [}] and have: '+dbo.json_Message(@json,@p))
				return;																	
				end
			update @hierarchy set pend=@start where id=@id
			return
			end
		
		-- Handle Arrays

		if (@kind='[') begin
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,@name,'ARRAY',@start,@start+1,'')
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) select id,parent,name,kind,ppos,pend,value from dbo.json_Array(@id+1,@id,@start+1,@json)
			if exists(select * from @hierarchy where kind='ERROR') return
			
			select @p=max(pend) from @hierarchy
			set @start = dbo.json_SkipUntil(@json,@p,']')
			
			if (@start=0) begin
				insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,'','ERROR',@p,@p,'Expected []] and have: '+dbo.json_Message(@json,@p))
				return;																	
				end
			update @hierarchy set pend=@start where id=@id
			return
			end
		
		-- Handle NUmbers
		if (@kind='-' or (@kind>='0' and @kind<='9')) begin
			select @p=p2,@value=value from dbo.json_Number(@json,@start)
			if (@p=-1) begin
					insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,'','ERROR',@p,-1,isnull(@value,'Bad number'))
					return;																	
				end
				insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,@name,'NUMBER',@start,@p,@value)
			return
		end	
		
		-- Handle TRUE
		if (upper(substring(@json,@start,4))='TRUE') begin
				insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,@name,'BOOL',@start,@start+4,'1')
				return
		end
		
		-- Handle FALSE
		if (upper(substring(@json,@start,5))='FALSE') begin
				insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,@name,'BOOL',@start,@start+5,'0')
				return
		end
		
		insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(0,@parent,@name,'ERROR',@start,@start,'Unexpected token '+@kind)
		return		
end

GO
/****** Object:  UserDefinedFunction [dbo].[json_Message]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/****** json_Message  ******/
CREATE FUNCTION [dbo].[json_Message](@JSON NVARCHAR(MAX),@start int)
RETURNS NVARCHAR(MAX)
AS
	BEGIN
		set @start=dbo.json_Skip(@json,@start)
		if (@start=0) return '** END OF TEXT **'
		return substring(@json,@start,30)
	end

GO
/****** Object:  UserDefinedFunction [dbo].[json_Name]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



/************* json_Name *********************/

CREATE FUNCTION [dbo].[json_Name](@start int,@JSON NVARCHAR(MAX))
RETURNS @hierarchy table
(
  kind nvarchar(5),name nvarchar(max),pend int
)
AS BEGIN
	declare @p1 int
	declare @name nvarchar(max)
	
	set @p1 = dbo.json_SkipUntil(@json,@start,'"')
	if (@p1=0) begin
			insert into @hierarchy(kind,name,pend) values('ERROR','Expected ["] in property name and have: '+substring(@json,@start,10),@start)
			return;																	
			end
	select @name=value,@start=p2 from dbo.json_String(@json,@p1)
	if (@start=-1) begin
			insert into @hierarchy(kind,name,pend) values('ERROR',@name,@p1)
			return;																	
			end
	set @p1 = dbo.json_SkipUntil(@json,@start,':')
	if (@p1=0) begin
			insert into @hierarchy(kind,name,pend) values('ERROR','Expected [:] after name and have: '+substring(@json,@start,10),@start)
			return;																	
			end
	
	set @start = dbo.json_Skip(@json,@p1)
	if (@start=0) begin
			insert into @hierarchy(kind,name,pend) values('ERROR','Expected a value after [:] and have: '++substring(@json,@p1,10),@p1)
			return;																	
			end
	insert into @hierarchy(kind,name,pend) values('OK',@name,@start)
	return
END

GO
/****** Object:  UserDefinedFunction [dbo].[json_Natural]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


CREATE FUNCTION [dbo].[json_Natural](@JSON NVARCHAR(MAX),@start int)
RETURNS NVARCHAR(MAX)
AS BEGIN
    declare @value NVARCHAR(MAX)=''
    while SUBSTRING(@json,@start,1) >= '0' AND SUBSTRING(@json,@start,1) <= '9' BEGIN
	    set @value=@value +SUBSTRING(@json,@start,1)
		set @start=@start+1
		end
    return @value
END

GO
/****** Object:  UserDefinedFunction [dbo].[json_Number]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/************* json_Number *********************/
CREATE FUNCTION [dbo].[json_Number](@JSON NVARCHAR(MAX),@start int)
RETURNS @data table
(
	p1 int,
	p2   int,
	value nvarchar(max)
)
AS BEGIN
	declare @p1 int =@start
	declare @value nvarchar(max)=''
	declare @cof   nvarchar(max)=''
	declare @v nvarchar(max)=''

	insert @data(p1,p2,value) select 1,@start+1,100

	-- Parse NEGATIVE sign
	if (SUBSTRING(@json,@start,1)='-') begin
		set @value='-'
		set @start=@start+1
	end 

    -- Parse integer part of number
	set @v=dbo.json_Natural(@JSON,@start)
    set @value=@value+@v
    set @start=@start+len(@v)
	
	-- Let's handle .
	if (SUBSTRING(@json,@start,1)='.') begin
		set @value=@value+'.'
		set @start=@start+1
	    set @v=dbo.json_Natural(@JSON,@start)
        if (@v='') begin 
			insert @data(p1,p2,value) select @p1,-1,'Expected fractional part when parsing Number'
			return
			end		
        set @value=@value+@v
        set @start=@start+len(@v)
        end
    -- If this is an EXPO
	if (lower(SUBSTRING(@json,@start,1))='e') begin
		set @start=@start+1
	    set @cof=SUBSTRING(@json,@start,1)
        if (@cof!='+') and (@cof!='-') and (@cof<'0') AND (@cof > '9') begin
			insert @data(p1,p2,value) select @p1,-1,'Expected sign in coeficient part when parsing Number'
			return
        end
        if (@cof='+') or (@cof='-') set @start=@start+1 else set @cof='+'
	    set @v=dbo.json_Natural(@JSON,@start)
        if (@v='') begin 
			insert @data(p1,p2,value) select @p1,-1,'Expected coeficient part when parsing Number'
			return
			end		
        set @start=@start+len(@v)
        -- Make a numeric value
        set @value=convert(nvarchar(max),convert(float,@value+'E'+@cof+@v))

    end


	
	-- Insert value
	insert @data(p1,p2,value) select @p1,@start,@value
	return
end

GO
/****** Object:  UserDefinedFunction [dbo].[json_Object]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO





/************* json_Object *********************/
-- When entering here we know for sure that we are after '{'

CREATE FUNCTION [dbo].[json_Object](@id int,@parent int, @start int,@JSON NVARCHAR(MAX))
RETURNS @hierarchy table
(
  id int,parent int,name nvarchar(2000),kind nvarchar(10),ppos int,pend int,value nvarchar(MAX) NOT NULL
)
AS
	BEGIN
		declare @pos int,@m int,@b int=@start
		-- Look for first item after {		
		set @pos=dbo.json_Skip(@json,@start)

		-- IF arrived to the end
		if (@pos = 0) begin
			--insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,'','JAR',@start,-1,'['+right(@json,1)+']')
			if (right(@json,1)='}') return -- If found because this is an empty object then do not raise an error
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(@id,@parent,'','ERROR',@start,-1,'Wrong object definition')
			return
			end		
		-- If is } then we have anempty object, lets return
		if (substring(@json,@pos,1)='}') return

		-- Enter endless loop
		while (1=1) begin
			-- Insert item into hierarchy
			insert into @hierarchy(id,parent,name,kind,ppos,pend,value) select id,parent,name,kind,ppos,pend,value from [dbo].[json_Item](@id,@parent,@pos,@json,1)
			-- If nothing was inserted then return	
			if not exists(select * from @hierarchy) begin
					insert into @hierarchy(id,parent,name,kind,ppos,pend,value) values(0,0,'','ERROR',@pos,-1,'Unexpected error')
					break
					end
			-- If an error happened then return
			if exists(select * from @hierarchy where kind='ERROR') break
			-- Get MAX id of inserted objects and ADD 1. This sets the new ID
			select @id=max(id)+1 from @hierarchy 
			-- Get latest position of readed object
			select @m=max(pend) from @hierarchy 
			-- Skip after 

			set @pos = dbo.json_Skip(@json,@m)
			-- If we do not have a [,] then exit loop
			if (substring(@json,@pos,1)!=',') break
			-- Move after ,
			set @pos = dbo.json_Skip(@json,@pos+1)
			end
	return
	end

GO
/****** Object:  UserDefinedFunction [dbo].[json_Parse]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/****** json_Parse  ******/
CREATE FUNCTION [dbo].[json_Parse](@json nvarchar(max))
RETURNS @data TABLE
	(
	id int NOT NULL,
	parent int NOT NULL,
	name nvarchar(100) NOT NULL,
	kind nvarchar(10) NOT NULL,
	value nvarchar(MAX) NOT NULL)
AS
BEGIN
	declare @start int = 1
	set @start = dbo.json_Skip(@json,@start)
	
	insert into @data(id,parent,name,kind,value)
	select id,parent,name,kind,value from dbo.json_Item(1,0,@start,@json,0)
	return
END

GO
/****** Object:  UserDefinedFunction [dbo].[json_Skip]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



/****** json_Skip  ******/
CREATE  FUNCTION [dbo].[json_Skip](@JSON NVARCHAR(MAX),@start int)
returns int
as
begin
	if (@start>=len(@json)) return 0
	while ((@start<=len(@json)) and (ascii(substring(@json,@start,1))<=32)) set @start=@start+1
	if (@start>len(@json)) return 0
	return @start
end

GO
/****** Object:  UserDefinedFunction [dbo].[json_SkipUntil]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/****** json_SkipUntil  ******/
CREATE FUNCTION [dbo].[json_SkipUntil](@JSON NVARCHAR(MAX),@start int,@what NVARCHAR(MAX))
returns int
as
begin
	while ((@start<=len(@json)) and (ascii(substring(@json,@start,1))<=32)) begin
		set @start=@start+1
		end
	if (@start>len(@json)) return 0
	if (substring(@json,@start,len(@what))!=@what) return 0
	return @start+1
end

GO
/****** Object:  UserDefinedFunction [dbo].[json_String]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/************* json_String *********************/
CREATE FUNCTION [dbo].[json_String](@JSON NVARCHAR(MAX),@start int)
RETURNS @data table
(
	p1 int,
	p2   int,
	value nvarchar(max)
)
AS BEGIN
	declare @p1 int =@start
	declare @value nvarchar(max)=''
	while (@start<=len(@json) and SUBSTRING(@json,@start,1)!='"') begin
		if (SUBSTRING(@json,@start,1)='\') set @start=@start+1
		if @start<len(@json) set @value=@value+SUBSTRING(@json,@start,1)
		set @start=@start+1
		end
	if (@start>len(@json)) begin
		insert @data(p1,p2,value) select @p1,-1,'Unterminated string'
		return
		end 
	insert @data(p1,p2,value) select @p1,@start+1,@value 
	return
end


GO
/****** Object:  UserDefinedFunction [dbo].[json_toJson]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/************* json_toJson *********************/



CREATE  FUNCTION [dbo].[json_toJson](@data pJsonData READONLY,@node int)
returns nvarchar(max)
as begin
    declare @kind   nvarchar(max)
    declare @value  nvarchar(max)

    select @kind=kind,@value=value from @data where id=@node
    if (@kind='STRING') return '"'+@value+'"'
    if (@kind='NUMBER') return @value
    if (@kind='BOOL') begin
        if @value=1 return 'True' else return 'False'
        end
    if (@kind='OBJECT') begin
        set @value=''
        SELECT @value= @value+ ',"'+Name +'":'+dbo.json_toJson(@data,id) FROM @data where parent=@node
        return '{'+iif(@value='','',substring(@value,2,len(@value)-1))+'}'
        end
    if (@kind='ARRAY') begin
        set @value=''
        SELECT @value= @value+ ','+dbo.json_toJson(@data,id) FROM @data where parent=@node
        return '['+iif(@value='','',substring(@value,2,len(@value)-1))+']'
        end
    return cast('Unkown KIND in Json Data' as int);
    return '*ERROR*'
end

GO
/****** Object:  UserDefinedFunction [dbo].[json_Value]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/****** json_Value  ******/
CREATE FUNCTION [dbo].[json_Value](@json nvarchar(max),@path nvarchar(max))
RETURNS nvarchar(MAX) 
AS
BEGIN
    declare @data pJsonData
    declare @p1   TABLE(id int identity(1,1),name nvarchar(max))
    
    insert into @data       select id,parent,name,kind,value from dbo.json_Parse(@json)
    set @path=ltrim(rtrim(@path))
    if @path!='' insert into @p1(name)   select * from dbo.ufn_Split(@path,'.')
    
    declare @c0    int =0
    declare @cur    int =null
    declare @step   int =1
    declare @max    int =1
    declare @v      nvarchar(max)
    declare @v2      nvarchar(max)
    declare @kind   nvarchar(max)
    select  @max=max(id) from @p1
    select @c0=id,@cur=id, @kind=kind,@v=value from @data where parent=0           -- Current object is the one with parent =0
    if @cur is null return null                                             -- Should not happen
    while (@step<=@max and @c0 is not null) begin
        select @v=name from @p1 where id=@step
        set @c0 =null
        if (IsNumeric(@v)=1) begin
                if (@kind!='ARRAY') return cast('Using index in non array JSON' as int);
                set @v2=@v
                SELECT @c0=ID,@cur=ID,@kind=kind,@v=value FROM @data where parent=@cur ORDER BY ID OFFSET convert(int,@v) ROWS FETCH FIRST 1 ROW ONLY
        end
        else begin
                if (@kind!='OBJECT') return cast('Using property name in a non-object JSON' as int);
                SELECT @c0=ID,@cur=ID,@kind=kind,@v=value FROM @data where parent=@cur and name=@v
        end
        set @step=@step+1
    end
    if @c0 is null return null
    if (@kind='OBJECT' or @kind='ARRAY') return dbo.json_toJson(@data,@cur)
    return @v
END



GO
/****** Object:  UserDefinedFunction [dbo].[json_Value2]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/****** json_Value  ******/
CREATE FUNCTION [dbo].[json_Value2](@data pJsonData READONLY,@path nvarchar(max))
RETURNS nvarchar(MAX) 
AS
BEGIN
    declare @p1   TABLE(id int identity(1,1),name nvarchar(max))
    
    set @path=ltrim(rtrim(@path))
    if @path!='' insert into @p1(name)   select * from dbo.ufn_Split(@path,'.')
    
    declare @c0    int =0
    declare @cur    int =null
    declare @step   int =1
    declare @max    int =1
    declare @v      nvarchar(max)
    declare @v2      nvarchar(max)
    declare @kind   nvarchar(max)
    select  @max=max(id) from @p1
    select @c0=id,@cur=id, @kind=kind,@v=value from @data where parent=0           -- Current object is the one with parent =0
    if @cur is null return null                                             -- Should not happen
    while (@step<=@max and @c0 is not null) begin
        select @v=name from @p1 where id=@step
        set @c0 =null
        if (IsNumeric(@v)=1) begin
                if (@kind!='ARRAY') return cast('Using index in non array JSON' as int);
                set @v2=@v
                SELECT @c0=ID,@cur=ID,@kind=kind,@v=value FROM @data where parent=@cur ORDER BY ID OFFSET convert(int,@v) ROWS FETCH FIRST 1 ROW ONLY
        end
        else begin
                if (@kind!='OBJECT') return cast('Using property name in a non-object JSON' as int);
                SELECT @c0=ID,@cur=ID,@kind=kind,@v=value FROM @data where parent=@cur and name=@v
        end
        set @step=@step+1
    end
    if @c0 is null return null
    if (@kind='OBJECT' or @kind='ARRAY') return dbo.json_toJson(@data,@cur)
    return @v
END




GO
/****** Object:  UserDefinedFunction [dbo].[json_XmlToJson]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE FUNCTION [dbo].[json_XmlToJson](@XmlData xml)
RETURNS nvarchar(max)
AS
BEGIN
  declare @m nvarchar(max)
  SELECT @m='['+Stuff
  (
     (SELECT theline from
    (SELECT ','+' {'+Stuff
       (
              (SELECT ',"'+coalesce(b.c.value('local-name(.)', 'NVARCHAR(255)'),'')+'":'+
                      case when b.c.value('count(*)','int')=0 
                      then dbo.[qfn_JsonEscape](b.c.value('text()[1]','NVARCHAR(MAX)'))
                      else dbo.qfn_XmlToJson(b.c.query('*'))
                      end
                 from x.a.nodes('*') b(c)                                                                
                 for xml path(''),TYPE).value('(./text())[1]','NVARCHAR(MAX)')
               ,1,1,'')+'}'
          from @XmlData.nodes('/*') x(a)
       ) JSON(theLine)
       for xml path(''),TYPE).value('.','NVARCHAR(MAX)')
      ,1,1,'')+']'
   return @m
END
GO
/****** Object:  UserDefinedFunction [dbo].[ufn_split]    Script Date: 6/16/2015 3:12:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

-- FROM http://sqlperformance.com/2012/07/t-sql-queries/split-strings
CREATE FUNCTION [dbo].[ufn_split]
(
   @List       NVARCHAR(MAX),
   @Delimiter  NVARCHAR(255)
)
RETURNS TABLE
WITH SCHEMABINDING
AS
   RETURN 
   (  
      SELECT Item = y.i.value('(./text())[1]', 'nvarchar(4000)')
      FROM 
      ( 
        SELECT x = CONVERT(XML, '<i>' 
          + REPLACE(@List, @Delimiter, '</i><i>') 
          + '</i>').query('.')
      ) AS a CROSS APPLY x.nodes('i') AS y(i)
   );

GO

USE [FCS_DAT]
GO
/****** Object:  StoredProcedure [dbo].[usp_forunco]    Script Date: 04/05/2019 17:20:52 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/*-------------------------------------------------
	Author:			Yves Mauron
	Create date:	31.12.2018
	Description:	
	This procedure aims at preprocessing any time-
	series data and feeding it to an forecasting
	algorithm in R. Note that the procedure uses
	the default configuration of the forunco
	algorithms and that R specific paramters/
	functions can or have to be adjusted in the
	sp_execute_external_script call at the end of 
	this script. However, if one wants the user 
	to have the ability of setting these paramters 
	with each call of the procedure, please add 
	them to the params attribute and, ultimately 
	to the parameters of the stored procedure.

	The main functionality of proprocessing
	tasks in this script is to make sure that 
	the input data for the algorithms has no missing
	data points; ultimately inserting additional 
	0 values for dates that have no values. Also,
	the script provides the functionality for 
	temporal aggregation and calendar adjustments
	to give the time-series forecasting algorithms
	cleaner data that can be further adjusted 
	in R . The overall data-flow can be 
	illustrated as follows:

						o start
						|
						o paramter check
						|
						o  pre-processing-options
					   / \
	even distributes  o	  o none or avg 
	the agg_levels	  |	  |	- none sums the values
	evenly, meaning	  |	  |	  of each agg_level (no 
	that each month	  |	  |	  adjustment)
	has the same	  |	  |	- avg averages the daily
	amount of days	  |	  |	  values of each agg_level;
					  |	  |	  ultimately returns the 
					  |	  |	  average per business day
					  |	  |	  in a resprective month
					   \ /
					    o check if values are missing,
						| if yes, insert 0 with the 
						| correct date.
						|
						o pass data to R script
						|
						o get results and store them
						| in fcs.results.
						|
						o end; i.e. further custom
						  post-processing, i.e. in case
						  of avg. you can determine the
						  number of business days (special
						  opening days) and multiply the 
						  daily average with the number of 
						  open days.

	For additional information about R functions in
	general or the functions provided by forunco,
	please see the corresponding package documentation.
	-------------------------------------------------*/

ALTER PROCEDURE [dbo].[usp_forunco]
	@schema as sysname, -- schema name
	@table as sysname, -- table name
	@date_col as nvarchar(50), -- column that should be used as date (i.e. Payment Date)
	@value_col as nvarchar(50), -- column that should be used for the values (i.e. Sales Amount)
	@group_col as nvarchar(50), -- the column that groups the series (i.e. ProductNr)
	@agg_level as nvarchar(15) = N'Month', -- aggregation level (Year, Quarter, Month, Week, Day)
	@agg_date_fun as nvarchar(3) = N'MIN', -- aggregation function to be used for date aggregation (wheter to put the date at the end or the beginning of the period 1.1 or 31.1)
	@calendar_adjustment as nvarchar(5) = N'even', -- calendar adjustment (avg, even, none); see above
	@check_missing_data as bit = 0, -- if lengthy procedure should take care of missing values, default NO!
	@omit_leap_days as bit = 0, -- if leap days should be removed, strongy advisable for even and none, with avg you can remove it.
	@horizons as int = 18 -- the number of horizons (time-steps) to predict
AS
BEGIN
	-- SET NOCOUNT ON 
	SET NOCOUNT ON;

	DECLARE
		@err_msg as nvarchar(255) -- placeholder for error messages

	BEGIN TRY
		BEGIN TRAN
			/*-------------------------------------------------
				Declare additional variables or adjust 
				variables according to data type.
			  -------------------------------------------------*/
			 DECLARE
				@sql_stmt as nvarchar(max), -- placeholder for dynamic sql statements
				@q as char = char(39), -- quotation character
				@days_in_year numeric(5, 2) = CASE @omit_leap_days WHEN 1 THEN 365 ELSE 365.25 END,
				@frequency numeric(5, 2)= 
						CASE LOWER(@agg_level)
							WHEN 'year' THEN 1
							WHEN 'quarter' THEN 4
							WHEN 'month' THEN 12
							WHEN 'week' THEN 365.25/7
							WHEN 'day' THEN 365.25
							ELSE 1 END;
			SET	@calendar_adjustment = CASE WHEN @agg_level NOT IN ('week', 'day')
											THEN @calendar_adjustment 
											ELSE 'none' END;
			
			/*-------------------------------------------------
				check input parameters and resulting data
			  -------------------------------------------------*/
			-- check if table exists in defined schema
			IF NOT EXISTS(SELECT * 
				  FROM INFORMATION_SCHEMA.TABLES 
				  WHERE TABLE_SCHEMA = @schema 
				  AND TABLE_NAME = @table)
			BEGIN
				-- set error message and raise error
				SET @err_msg = CONCAT('usp_forunco_forecast, no table with name: ', @table, ' found in schema: ', @schema);
				RAISERROR (15600, -1, -1, '');
			END
			
			-- drop tempdb sourceTable if it exists
			DROP TABLE IF EXISTS #sourceTable
			-- store candidate source columns for later approval
			SELECT 
				 c.TABLE_SCHEMA
				,c.TABLE_NAME
				,c.COLUMN_NAME
				,c.IS_NULLABLE
				,c.DATA_TYPE
			INTO #sourceTable
			FROM  INFORMATION_SCHEMA.COLUMNS c 
			INNER JOIN SYSOBJECTS o 
				ON c.TABLE_NAME = o.name 
			INNER JOIN sys.schemas s 
				ON o.uid = s.schema_id 
			LEFT JOIN sys.all_columns c2 
				ON o.id = c2.object_id 
			AND c.COLUMN_NAME = c2.name 
			WHERE c.TABLE_NAME = @table
			AND c.TABLE_SCHEMA = @schema
			AND c.COLUMN_NAME IN (@date_col, @value_col, @group_col)
			AND c2.is_computed = 0;
			
			-- count is deterministic, as no table can have two columns with the same name
			IF (SELECT COUNT(*) FROM #sourceTable) != 3
			BEGIN
				-- set error message and raise error
				SET @err_msg = CONCAT('usp_forunco_forecast, table: ',  @schema, '.', @table, 
									  ' does not have all of the following columns in its definition: ', @date_col, ', ', @value_col, ', ', @group_col, 
									  '. Furthermore, please note that computed columns are not considered.');
				RAISERROR (15600, -1, -1, '');
			END
			
			-- check if @datecol is really a date
			IF (SELECT LOWER(DATA_TYPE) FROM #sourceTable WHERE COLUMN_NAME = @date_col) NOT IN ('date', 'datetime', 'datetime2')
			BEGIN
				-- set error message and raise error
				SET @err_msg = CONCAT('usp_forunco_forecast, table: ',  @schema, '.', @table, 
									  ' column: ', @date_col, ' needs to be a date');
				RAISERROR (15600, -1, -1, '');
			END
			
			-- check if value column is a number
			IF (SELECT LOWER(DATA_TYPE) FROM #sourceTable WHERE COLUMN_NAME = @value_col) NOT IN ('int', 'numeric', 'decimal', 'bigint', 'float')
			BEGIN
				-- set error message and raise error
				SET @err_msg = CONCAT('usp_forunco_forecast, table: ',  @schema, '.', @table, 
									  ' column: ', @date_col, ' needs to be a date');
				RAISERROR (15600, -1, -1, '');
			END
			
			-- check if frequency is a valid one
			IF (SELECT LOWER(@agg_level)) NOT IN ('year', 'quarter', 'month', 'week'/*, 'day'*/)
			BEGIN
				-- set error message and raise error
				SET @err_msg = 'usp_forunco_forecast, parameter @agg_level wrongfully set, options are: year, quarter, month, week.
								Note that yearly means one occurence per year, quarter four, month 12 and week 52.';
				RAISERROR (15600, -1, -1, '');
			END
			
			-- check if aggregation function for date is a valid one
			IF LOWER(@agg_date_fun) NOT IN ('min', 'max', 'avg')
			BEGIN
				-- set error message and raise error
				SET @err_msg = 'usp_forunco_forecast, parameter @agg_date_fun wrongfully set, options are: min, max and avg.
								Note that this funciton will set the date of the forecast when the data is being aggregated, when
								choosing min, the first day is selected, when max is selected, the last day of e.g. the month is
								selected.';
				RAISERROR (15600, -1, -1, '');
			END
			
			-- check if calendar distribution is a valid one
			IF LOWER(@calendar_adjustment) NOT IN ('avg', 'even', 'none')
			BEGIN
				-- set error message and raise error
				SET @err_msg = 'usp_forunco_forecast, parameter @calendar_adjustment wrongfully set, options are: avg, even.
								Note that even distributes evenly (e.g. 365/12 for monthly data) and avg takes the daily average
								of the true months and adjusts the value later according to the corresponding day count in the month.
								Finally, none just aggregates according calendar month without any adjustments.';
				RAISERROR (15600, -1, -1, '');
			END
			
			/*-------------------------------------------------
				Make sure that all dates have a value and that
				the data is aggreagted on the correct level.
				Note that interpolation of outliers or 
				intermittend demand (0 values) is taken care 
				of in R. 
			  -------------------------------------------------*/
			
			-- get the sum for each day
			DROP TABLE IF EXISTS #sourceDataDay 
			CREATE TABLE #sourceDataDay (
				TSN NVARCHAR(255),
				[Date] DATE,
				[Value] numeric(28,4)
			)
			SET @sql_stmt = '
			INSERT INTO #sourceDataDay
			SELECT 
				 ' + @group_col + ' AS TSN
				,' + @date_col + ' AS [Date]
				,SUM(' + @value_col + ') AS [Value]
			FROM [' + @schema + '].[' + @table + ']
			GROUP BY 
				 ' + @group_col + '
				,' + @date_col;
			
			EXEC(@sql_stmt)
			
			-- perfrom calendar adjustment according to user input
			-- and insert data in temp table source data
			IF @calendar_adjustment in ('avg', 'none')
			BEGIN 
				/*
					AVG:
					avg takes the average of the days for each month and predicts
					the average, the result can then be recevied by multiplying
					this result with the corresponding business day of the month in 
					question; this method is ususally preferable, as business days
					can vary for different years and thus cause noise and consequently 
					hurt the model fitting process.
					
					None:
					none makes no adjustments and just sums the values according to the 
					calendar object (e.g. Month). 
				*/
				SET @sql_stmt = '
				WITH dateGroups AS (
					 SELECT 
							 TSN
							,[Date]
							,CASE ' + @q + LOWER(@agg_level) + @q + '
								WHEN ' + @q + 'year' + @q + ' THEN YEAR([Date]) 
								ELSE (YEAR([Date]) * 100) + ' + @agg_level + '([Date]) END AS DateGroup
							,[Value] AS [Value]
					 FROM #sourceDataDay
				)
				INSERT INTO #sourceData
				SELECT 
					TSN
					,' + @agg_date_fun + '([Date]) AS [Date]
					,DateGroup AS DateGroup
					,CASE ' + @q + LOWER(@calendar_adjustment) + @q + '
						WHEN ' + @q + 'avg' + @q + ' THEN AVG([Value]) 
						ELSE SUM([Value]) 
						END AS [Value]
				FROM dateGroups
				';
			END
			ELSE IF @calendar_adjustment = 'even' -- explicit for readability
			BEGIN
				/*
					even distributes the month independently from the calendar evently,
					meaning that for monthly data (frequency 12), there will be 12 month,
					all of which will have the same amount of days. This can be and is 
					frequently used in competition data to give each month the same weight
					and thus reduce the noise caused by varying business days in a month.
					Generally 
				 */
				SET @sql_stmt = '
				WITH dateGroups AS (
					 SELECT 
							 TSN
							,[Date]
							,((YEAR([Date]) * 100) + 
								CEILING(DATEPART(DAYOFYEAR, [Date]) / 
								(' + CAST(@days_in_year AS NVARCHAR(6)) + ' / ' + CAST(@frequency AS NVARCHAR(6)) + '))) AS DateGroup
							,[Value] AS [Value]
					 FROM #sourceDataDay
				)
				INSERT INTO #sourceData
				SELECT 
					TSN
					,' + @agg_date_fun + '([Date]) AS [Date]
					,DateGroup AS DateGroup
					,SUM([Value]) AS [Value]
				FROM dateGroups
				';
			END
			
			-- leaf year adjustment or not	
			IF @omit_leap_days = 1 
			BEGIN
				SET @sql_stmt = @sql_stmt +
				'WHERE NOT (MONTH([Date]) = 2 AND DAY([Date]) = 29) 
				'
			END
			-- grouping
			SET @sql_stmt = @sql_stmt +
			'GROUP BY TSN, DateGroup
			 ORDER BY TSN, [Date]'
			
			-- aggregate data
			DROP TABLE IF EXISTS #sourceData
			CREATE TABLE #sourceData (
				TSN nvarchar(255),
				[Date] Date,
				DateGroup INT,
				[Value] numeric(28,4)
			);
			EXEC(@sql_stmt)
			
			DROP TABLE IF EXISTS #dateSpan
			CREATE TABLE #dateSpan (
				 TSN NVARCHAR(255)
				,StartDate Date
				,EndDate Date
				,ActNumberOfObs INT
				,TgNumberOfObs INT
			);
			SET @sql_stmt = '
		    WITH tempData AS (
				 SELECT 
						 TSN
						,MIN([Date]) AS StartDate
						,MAX([Date]) AS EndDate
						,COUNT(*) AS ActNumberOfObs
				 FROM #sourceData
				 GROUP BY TSN
			)
			INSERT INTO #dateSpan
			SELECT 
				TSN
				,StartDate
				,EndDate
				,ActNumberOfObs
				,(DATEDIFF(' + @agg_level + ', StartDate, EndDate) + 1) TgNumberOfObs
			FROM tempData;
			';
			EXEC(@sql_stmt);
		
			-- if actual and target number of observations is the same
			-- then the time series is good to go.
			DROP TABLE IF EXISTS #inputData
			CREATE TABLE #inputData (
				[TSN] nvarchar(255),
				[Date] Date,
				[Value] numeric(18,4),
				[Frequency] numeric(10,4),
				[Horizon] INT
			);
	
			SET @sql_stmt = '
			INSERT INTO #inputData
			SELECT 
			 	 TSN 
				,[Date]
				,[Value]
				,' + CAST(@frequency AS NVARCHAR(25)) + ' as Frequency
				,' + CAST(@horizons AS NVARCHAR(25)) + ' as Horizons
			FROM #sourceData';

			IF @check_missing_data = 1
			BEGIN 
				 SET @sql_stmt = @sql_stmt + '
					WHERE TSN IN (SELECT TSN FROM #dateSpan WHERE ActNumberOfObs = TgNumberOfObs);
					';
			END
			-- write data into final table
			EXEC(@sql_stmt);
	
			-- check if some time series show missing time steps
			IF EXISTS (SELECT * FROM #dateSpan WHERE ActNumberOfObs != TgNumberOfObs) AND @check_missing_data = 1
			BEGIN
				-- lengthy procedure for this special case
				-- get max dates to create a miniDayDim that covers all dates of all time series
				DECLARE
					@startDate nvarchar(20) = (SELECT MIN(StartDate) FROM #dateSpan WHERE ActNumberOfObs != TgNumberOfObs),
					@endDate nvarchar(20) = (SELECT MAX(EndDate) FROM #dateSpan WHERE ActNumberOfObs != TgNumberOfObs);
			
				-- create tmpdb for mini day dim 
				DROP INDEX IF EXISTS tempdb.#miniDayDim.cx_tempMiniDateDim
				DROP TABLE IF EXISTS tempdb.#miniDayDim
				CREATE TABLE #miniDayDim (
					FullDate DATE,
			 		[Year] INT,
			 		[Quarter] INT,
			 		[Month] INT,
			 		[Week] INT,
			 		[Day] INT,
					[MathDay] FLOAT
				);
				-- declare variables for dynamic sql
				DECLARE 
					@stringEndDate VARCHAR(20) = CAST(@endDate AS nvarchar(10)),
					@ndays nvarchar(20) = DATEDIFF(DAY, @startdate, @enddate);
				-- generate set based day dimension
				SET @sql_stmt = '
						WITH x AS (
							SELECT 
								n 
							FROM (
								VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9)
							) numbers (n)
						), offSet AS (
							SELECT 
								ones.n + (10 * tens.n) + (100 * hundreds.n) + (1000 * thousands.n) + (10000 * tenthousends.n) AS n
							FROM x ones
							CROSS JOIN x tens
							CROSS JOIN x hundreds
							CROSS JOIN x thousands
							CROSS JOIN x tenthousends
						), dateParts AS (
							SELECT 
								DATEADD(DAY, - o.n, ' + @q + @stringEndDate + @q + ') FullDate,
								YEAR(DATEADD(DAY, - o.n, ' + @q + @stringEndDate + @q + ')) YearPart, 
								DATEPART(QUARTER, DATEADD(DAY, - o.n, ' + @q + @stringEndDate + @q + ')) QuarterPart, 
								MONTH(DATEADD(DAY, - o.n, ' + @q + @stringEndDate + @q + ')) MonthPart,
								DATEPART(WEEK, ' + @q + @stringEndDate + @q + ' ) WeekPart,
								FORMAT(DATEADD(DAY, - o.n, ' + @q + @stringEndDate  + @q + '), ' + @q + 'yyyMMdd' + @q + ') Day
							FROM offSet	o	
							WHERE o.n <= ' + @ndays + '
						)
						INSERT INTO #miniDayDim
						SELECT 
							 FullDate
							,YearPart as [Year]
							,(YearPart * 10) + QuarterPart as [Quarter]
							,(YearPart * 100) + MonthPart as [Month]
							,(YearPart * 100) + WeekPart as [Week]
							,Day As [Day]
							,DATEPART(DAYOFYEAR, FullDate) * (365.25 / 365)
						FROM dateParts
						ORDER BY Day;
						CREATE CLUSTERED INDEX cx_tempMiniDateDim ON #miniDayDim(FullDate);';
				EXEC(@sql_stmt);
	
				-- Populate all time series on the lowest level
				-- possibly add addtional logic to split data if
				-- predictions on lower levels are required
				DROP TABLE IF EXISTS tempdb.#temporalAggregationLevel
				CREATE TABLE #temporalAggregationLevel (
					TSN NVARCHAR(255),
					DateSK INT,
					FullDate DATE,
					TempAggLevel INT
				)
				SET @sql_stmt = '
				INSERT INTO #temporalAggregationLevel
				SELECT 
			 		 ds.TSN 
			 		,ca.DateSK
			 		,ca.FullDate
			 		,ca.' + @agg_level + ' AS TempAggLevel
				FROM #dateSpan ds
				CROSS APPLY (
			 		SELECT 
			 			 Day DateSK
			 			,FullDate
			 			,[' + @agg_level + ']
			 		FROM #miniDayDim
			 		WHERE FullDate BETWEEN ds.StartDate AND ds.EndDate
				) ca
				WHERE ActNumberOfObs != TgNumberOfObs;'
				EXEC(@sql_stmt)
				
				SET @sql_stmt = '
				INSERT INTO #inputData
				SELECT 
					 ta.TSN 
					,' + @agg_date_fun + '(ta.FullDate) AS [Date]
					,SUM(ISNULL(sd.Value, 0))
					,' + CAST(@frequency AS NVARCHAR(25)) + ' as Frequency
					,' + CAST(@horizons AS NVARCHAR(25)) + ' as Horizons
				FROM #temporalAggregationLevel ta
				LEFT JOIN #sourceData sd
					ON ta.TSN = sd.TSN
					AND ta.DateSK = FORMAT(sd.Date, ' + @q + 'yyyMMdd' + @q + ')
				WHERE ta.TSN IN (SELECT TSN FROM #dateSpan WHERE ActNumberOfObs != TgNumberOfObs)
				GROUP BY 
					ta.TSN, 
					ta.TempAggLevel;
				';
				EXEC(@sql_stmt);
			END
			
			/* 
				Prepare input table and output table and
				execute R code.
			 */
			 
			IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'fcs')
			BEGIN
				EXEC('CREATE SCHEMA fcs;')
			END

			-- prepare input data
			DROP TABLE IF EXISTS fcs.InputData
			SELECT 
				TSN
				,[Date]
				,[Value]
				,[Frequency]
				,[Horizon]
			INTO fcs.InputData
			FROM #inputData

			-- prepare output table
			DROP TABLE IF EXISTS fcs.Result
			CREATE TABLE fcs.Result (
				Proc_id NVARCHAR(20),
				TSN NVARCHAR(50), 
				[Date] NVARCHAR(50),
				[Type] NVARCHAR(50),
				[Value] NUMERIC(28,4)
			)
			
			--
			INSERT INTO fcs.Result 
			EXEC sp_execute_external_script  
			 @language = N'R'  
			,@script = N'  
			  library(forunco);  
			  result <- rx_sql_forunco(
				connection_string = connection_string, 
				table = table,
				h = horizons,
				num_cores = 8,
			    levels = c(95),
			    methods = c("auto_ets", "auto_arima", "auto_thetaf"),
			    point_combination = "median",
			    pi_combination_upper = "median",
			    pi_combination_lower = "median",
			    pool_limit = 3,
			    error_fun = "se",
			    weight_fun = "inverse",
			    val_h = horizons,
			    sov_only = F,
			    max_years = 30,
			    val_min_years = 4,
			    cv_min_years = 5,
			    cv_max_samples = 3,
			    allow_negatives = F
			)'	
			,@input_data_1 = N''  
			,@output_data_1_name  = N'result'  
			,@params = N'@table NVARCHAR(50), @connection_string NVARCHAR(150), @horizons INT'
			,@connection_string = 'Server=LTYMA01\QWERTZ_INT;Database=FCS_DAT;UID=ruser;PWD=ruser;'
			,@table = 'fcs.InputData'
			,@horizons = @horizons;
	
		COMMIT TRAN
	END TRY
	BEGIN CATCH
		ROLLBACK TRAN
		RAISERROR (15600, -1, -1, @err_msg);
	END CATCH

END

USE master;
GO

IF DB_ID('Lab8DB') IS NOT NULL
BEGIN
  ALTER DATABASE Lab8DB SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
    DROP DATABASE Lab8DB;
END
GO

CREATE DATABASE Lab8DB
  ON PRIMARY
    ( NAME = Lab8DB_PrimaryData,
      FILENAME = 'E:\BaumDBdata\lab8\lab8db.mdf',
      SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
  LOG ON
    ( NAME = LogOne, FILENAME = 'E:\BaumDBdata\lab8\lab8dblog.ldf',
      SIZE = 5MB, MAXSIZE = 100MB, FILEGROWTH = 5MB)
GO

USE Lab8DB;
GO

CREATE TABLE PHARMACY (
    PharmacyID UNIQUEIDENTIFIER PRIMARY KEY DEFAULT NEWID(),
    PharmacyAddress NVARCHAR(200) NOT NULL,
    PharmacyName NVARCHAR(150) NOT NULL,
    Phone CHAR(10) NULL,
    Mail VARCHAR(254) NOT NULL
);
GO

INSERT INTO PHARMACY (PharmacyAddress, PharmacyName, Phone, Mail)
VALUES 
    (N'Москва, ЖК Кварталы 21/19, пр-д 44к3', N'Горздрав', '4996536277', 'feedback@gorzdrav.org'),
    (N'Москва, Пресненская наб., 12', N'36,6', '4957976336', 'feedback@366.ru'),
    (N'Москва, ЖК Садовые кварталы, ул. Ефремова 10', N'Магнит Аптека', '8002009002', 'apteka.online@magnit.ru'),
    (N'ул. Ленина, 1', N'Тестовая аптека', '9000000000', 'test@apteka.ru');  
GO
SELECT * FROM PHARMACY;
GO

-------------------------------------------------------------
-- 1. Хранимая процедура, результат выборки в виде курсора
-------------------------------------------------------------
CREATE PROCEDURE dbo.GetPharmacyCursor
    @PhCursor CURSOR VARYING OUTPUT
AS
BEGIN
    SET @PhCursor = CURSOR FOR
        SELECT PharmacyID, PharmacyName, PharmacyAddress
        FROM PHARMACY;

    OPEN @PhCursor;
END;
GO

DECLARE @MyCursor CURSOR;
EXEC dbo.GetPharmacyCursor @PhCursor = @MyCursor OUTPUT;

DECLARE @PhID1 UNIQUEIDENTIFIER, @PhName1 NVARCHAR(150), @PhAddr1 NVARCHAR(200);
FETCH NEXT FROM @MyCursor INTO @PhID1, @PhName1, @PhAddr1;

WHILE @@FETCH_STATUS = 0
BEGIN
    PRINT N'   ID: ' + CONVERT(NVARCHAR(36), @PhID1) + 
          N', Название: ' + @PhName1 + 
          N', Адрес: ' + @PhAddr1;
    FETCH NEXT FROM @MyCursor INTO @PhID1, @PhName1, @PhAddr1;
END

CLOSE @MyCursor;
DEALLOCATE @MyCursor;
GO
------------------------------------------------------------------------------
-- 2. Хранимая процедура 1., выборка осуществляется с формированием столбца, 
--    значение которого формируется пользовательской функцией
------------------------------------------------------------------------------
CREATE FUNCTION dbo.GetPharmacyDescription(@Name NVARCHAR(150), @Addr NVARCHAR(200))
RETURNS NVARCHAR(400)
AS
BEGIN
    RETURN @Name + N' — адрес: ' + @Addr;
END;
GO

CREATE PROCEDURE dbo.GetPharmacyCursorWithFunc
    @PhCursor CURSOR VARYING OUTPUT
AS
BEGIN
    SET @PhCursor = CURSOR FOR
        SELECT 
            PharmacyID,
            dbo.GetPharmacyDescription(PharmacyName, PharmacyAddress) AS FullDescription
        FROM PHARMACY;

    OPEN @PhCursor;
END;
GO

PRINT N'';
DECLARE @MyCursor2 CURSOR;
EXEC dbo.GetPharmacyCursorWithFunc @PhCursor = @MyCursor2 OUTPUT;

DECLARE @PhID2 UNIQUEIDENTIFIER, @FullDesc2 NVARCHAR(400);
FETCH NEXT FROM @MyCursor2 INTO @PhID2, @FullDesc2;

WHILE @@FETCH_STATUS = 0
BEGIN
    PRINT N'   ID: ' + CONVERT(NVARCHAR(36), @PhID2) + 
          N', Описание: ' + @FullDesc2;
    FETCH NEXT FROM @MyCursor2 INTO @PhID2, @FullDesc2;
END

CLOSE @MyCursor2;
DEALLOCATE @MyCursor2;
GO


-----------------------------------------------------------------
-- 3. Хранимая процедура 1., прокрутку возвращаемого курсора и 
--    выводящую сообщения ещё одной пользовательской функции
-----------------------------------------------------------------
CREATE FUNCTION dbo.IsLongName(@Name NVARCHAR(150))
RETURNS BIT
AS
BEGIN
    IF LEN(@Name) > 5 RETURN 1;
    RETURN 0;
END;
GO

CREATE PROCEDURE dbo.ProcessPharmacyCursor
AS
BEGIN
    DECLARE @Cur CURSOR;
    DECLARE @ID UNIQUEIDENTIFIER, @Name NVARCHAR(150), @Addr NVARCHAR(200);

    EXEC dbo.GetPharmacyCursor @Cur OUTPUT;

    FETCH NEXT FROM @Cur INTO @ID, @Name, @Addr;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        IF dbo.IsLongName(@Name) = 1
        BEGIN
            PRINT N'Аптека #' + CONVERT(NVARCHAR(36), @ID) +
                  N': ' + @Name + N', адрес: ' + @Addr;
        END;

        FETCH NEXT FROM @Cur INTO @ID, @Name, @Addr;
    END;

    CLOSE @Cur;
    DEALLOCATE @Cur;
END;
GO

PRINT N'';
EXEC dbo.ProcessPharmacyCursor;
GO

------------------------------------------------------------------------------
-- 4. Хранимая процедура 2., выборка формируется с помощью табличной функции
------------------------------------------------------------------------------

CREATE FUNCTION dbo.fnPharmacyTable()
RETURNS @PharmacyTable TABLE 
(
    PharmacyID UNIQUEIDENTIFIER,
    PharmacyName NVARCHAR(150),
    PharmacyAddress NVARCHAR(200),
    FullDescription NVARCHAR(400)
)
AS
BEGIN
    INSERT INTO @PharmacyTable (PharmacyID, PharmacyName, PharmacyAddress, FullDescription)
    SELECT 
        PharmacyID,
        PharmacyName,
        PharmacyAddress,
        dbo.GetPharmacyDescription(PharmacyName, PharmacyAddress) AS FullDescription
    FROM PHARMACY;
    
    RETURN;
END;
GO

/*
CREATE FUNCTION dbo.fnPharmacyTable()
RETURNS TABLE
AS
RETURN 
(
    SELECT 
        PharmacyID,
        PharmacyName,
        PharmacyAddress,
        dbo.GetPharmacyDescription(PharmacyName, PharmacyAddress) AS FullDescription
    FROM PHARMACY
);
GO
*/

CREATE PROCEDURE dbo.GetPharmacyCursor_TableFunc
    @PhCursor CURSOR VARYING OUTPUT
AS
BEGIN
    SET @PhCursor = CURSOR FOR
        SELECT PharmacyID, FullDescription
        FROM dbo.fnPharmacyTable();

    OPEN @PhCursor;
END;
GO

PRINT N'';
DECLARE @c CURSOR;
EXEC dbo.GetPharmacyCursor @PhCursor = @c OUTPUT;

DECLARE @id UNIQUEIDENTIFIER, @name NVARCHAR(150), @addr NVARCHAR(200);
FETCH NEXT FROM @c INTO @id, @name, @addr;
WHILE @@FETCH_STATUS = 0
BEGIN
	PRINT CONVERT(NVARCHAR(36), @id) + N' - ' + @name;
	FETCH NEXT FROM @c INTO @id, @name, @addr;
END;
CLOSE @c;
DEALLOCATE @c;

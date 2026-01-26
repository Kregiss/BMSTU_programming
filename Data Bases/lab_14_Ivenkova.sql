USE Lab13DB_1;
GO

IF OBJECT_ID(N'Pharmacy_Ver1', N'U') IS NOT NULL
    DROP TABLE Pharmacy_Ver1;
GO

CREATE TABLE Pharmacy_Ver1
(
    PharmacyId INT PRIMARY KEY,
    PharmacyName NVARCHAR(150) NOT NULL,
    Phone CHAR(10) NULL,
    Mail VARCHAR(254) NOT NULL
);
GO

USE Lab13DB_2;
GO

IF OBJECT_ID(N'Pharmacy_Ver2', N'U') IS NOT NULL
    DROP TABLE Pharmacy_Ver2;
GO

CREATE TABLE Pharmacy_Ver2
(
    PharmacyId INT PRIMARY KEY,
    PharmacyAddress NVARCHAR(200) NOT NULL
);
GO

USE Lab13DB_1;
GO

IF OBJECT_ID(N'PharmacyView_Ver', N'V') IS NOT NULL
    DROP VIEW PharmacyView_Ver;
GO

CREATE VIEW PharmacyView_Ver
AS
    SELECT
        p1.PharmacyId,
        p1.PharmacyName,
        p1.Phone,
        p1.Mail,
        p2.PharmacyAddress
    FROM Lab13DB_1.dbo.Pharmacy_Ver1 p1
        INNER JOIN Lab13DB_2.dbo.Pharmacy_Ver2 p2 
            ON p1.PharmacyId = p2.PharmacyId;
GO

IF OBJECT_ID(N'trg_InsertPharmacyView_Ver', N'TR') IS NOT NULL
    DROP TRIGGER trg_InsertPharmacyView_Ver;
GO

CREATE TRIGGER trg_InsertPharmacyView_Ver
ON PharmacyView_Ver
INSTEAD OF INSERT
AS
BEGIN
    INSERT INTO Lab13DB_1.dbo.Pharmacy_Ver1
        (PharmacyId, PharmacyName, Phone, Mail)
    SELECT PharmacyId, PharmacyName, Phone, Mail
    FROM inserted;
        
    INSERT INTO Lab13DB_2.dbo.Pharmacy_Ver2
        (PharmacyId, PharmacyAddress)
    SELECT PharmacyId, PharmacyAddress
    FROM inserted;
END;
GO

IF OBJECT_ID(N'trg_UpdatePharmacyView_Ver', N'TR') IS NOT NULL
    DROP TRIGGER trg_UpdatePharmacyView_Ver;
GO

CREATE TRIGGER trg_UpdatePharmacyView_Ver
ON PharmacyView_Ver
INSTEAD OF UPDATE
AS
BEGIN
    SET NOCOUNT ON;

    IF UPDATE(PharmacyId)
    BEGIN
        RAISERROR('Изменение первичного ключа PharmacyId запрещено', 16, 1);
        ROLLBACK TRANSACTION;
		RETURN;
    END
        
    UPDATE p1
    SET 
        p1.PharmacyName = i.PharmacyName,
        p1.Phone = i.Phone,
        p1.Mail = i.Mail
    FROM Lab13DB_1.dbo.Pharmacy_Ver1 p1
        INNER JOIN inserted i ON p1.PharmacyId = i.PharmacyId;
        
    UPDATE p2
    SET 
        p2.PharmacyAddress = i.PharmacyAddress
    FROM Lab13DB_2.dbo.Pharmacy_Ver2 p2
        INNER JOIN inserted i ON p2.PharmacyId = i.PharmacyId;
END;
GO

IF OBJECT_ID(N'trg_DeletePharmacyView_Ver', N'TR') IS NOT NULL
    DROP TRIGGER trg_DeletePharmacyView_Ver;
GO

CREATE TRIGGER trg_DeletePharmacyView_Ver
ON PharmacyView_Ver
INSTEAD OF DELETE
AS
BEGIN        
	SET NOCOUNT ON;
    DELETE FROM Lab13DB_1.dbo.Pharmacy_Ver1
    WHERE PharmacyId IN (SELECT PharmacyId FROM deleted);
    DELETE FROM Lab13DB_2.dbo.Pharmacy_Ver2
    WHERE PharmacyId IN (SELECT PharmacyId FROM deleted);
END;
GO

------------------------------
-- Проверка
------------------------------

PRINT N'1. Вставка данных в вертикально фрагментированное представление.';
GO

INSERT INTO PharmacyView_Ver
    (PharmacyId, PharmacyName, Phone, Mail, PharmacyAddress)
VALUES
    (100, N'Горздрав', '4996536277', 'feedback@gorzdrav.org', 
     N'Москва, ЖК Кварталы 21/19, 2-й Грайвороновский проезд, 44 к3, 1 этаж'),
    (101, N'36,6', '4957976336', 'feedback@366.ru', 
     N'Москва, Пресненская набережная, 12, 1 этаж'),
    (102, N'Магнит Аптека', '8002009002', 'apteka.online@magnit.ru', 
     N'Москва, ЖК Садовые кварталы, Улица Ефремова, 10 ст1');
GO

SELECT * FROM PharmacyView_Ver ORDER BY PharmacyId;
GO

PRINT N'3. Проверка данных в отдельных таблицах.';
GO

SELECT 'Pharmacy_Ver1 (Lab13DB_1)' AS TableName, * 
FROM Lab13DB_1.dbo.Pharmacy_Ver1 
ORDER BY PharmacyId;

SELECT 'Pharmacy_Ver2 (Lab13DB_2)' AS TableName, * 
FROM Lab13DB_2.dbo.Pharmacy_Ver2 
ORDER BY PharmacyId;
GO

PRINT N'4. Обновление данных через представление.';
GO

UPDATE PharmacyView_Ver
SET
    PharmacyAddress = N'Москва, обновленный адрес, 100',
    Mail = 'new-mail@gorzdrav.org'
WHERE PharmacyName = 'Горздрав';
GO

SELECT N'После обновления' AS Status, * 
FROM PharmacyView_Ver 
ORDER BY PharmacyId;
GO

PRINT N'6. Попытка обновления PharmacyId (Ошибка!).';
GO

BEGIN TRY
    UPDATE PharmacyView_Ver
    SET PharmacyId = 999
    WHERE PharmacyName = '36,6';
    PRINT N'Ошибка: обновление PharmacyId не должно было быть разрешено!';
END TRY
BEGIN CATCH
    PRINT N'Ожидаемая ошибка: ' + ERROR_MESSAGE();
END CATCH
GO

PRINT N'7. Удаление данных через представление.';
GO

DELETE FROM PharmacyView_Ver
WHERE PharmacyName = '36,6';
GO

SELECT * FROM PharmacyView_Ver 
ORDER BY PharmacyId;

SELECT * FROM Lab13DB_1.dbo.Pharmacy_Ver1 
ORDER BY PharmacyId;

SELECT * FROM Lab13DB_2.dbo.Pharmacy_Ver2 
ORDER BY PharmacyId;
GO

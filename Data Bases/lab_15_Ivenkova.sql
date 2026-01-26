USE Lab13DB_1;
GO

IF OBJECT_ID(N'MEDICAMENT', N'U') IS NOT NULL
    DROP TABLE MEDICAMENT;
GO

CREATE TABLE MEDICAMENT (
    MedicamentID INT IDENTITY(1,1) PRIMARY KEY,
    MedicamentGRLS Nvarchar(15) NOT NULL,
    MedicamentName Nvarchar(150) NOT NULL,
    Form Tinyint NOT NULL CHECK (Form BETWEEN 0 and 20),
    StorageLife Smalldatetime NOT NULL,
    Price Smallmoney NOT NULL CHECK (Price >= 0)
);
GO

INSERT INTO MEDICAMENT (MedicamentGRLS, MedicamentName, Form, StorageLife, Price)
VALUES
    (N'GRLS001', N'Парацетамол', 11, '2026-01-01', 150.00),
    (N'GRLS002', N'Ибупрофен', 5, '2027-05-01', 220.50);
GO

USE Lab13DB_2;
GO

IF OBJECT_ID(N'LINEMEDICAMENT', N'U') IS NOT NULL
    DROP TABLE LINEMEDICAMENT;
GO

CREATE TABLE LINEMEDICAMENT (
    LineID INT IDENTITY(1,1) PRIMARY KEY,
    OrderNumber INT NULL,
    MedicamentID INT NULL,
    "Count" SMALLINT NOT NULL CHECK ("Count" > 0),
    LineCost SMALLMONEY NOT NULL CHECK (LineCost >= 0)
);
GO

IF OBJECT_ID(N'trg_InsertLINEMEDICAMENT', N'TR') IS NOT NULL
    DROP TRIGGER trg_InsertLINEMEDICAMENT;
GO

CREATE TRIGGER trg_InsertLINEMEDICAMENT
ON LINEMEDICAMENT
AFTER INSERT
AS
BEGIN
    SET NOCOUNT ON;

    -- Проверка существования MedicamentID
    IF EXISTS (
        SELECT 1
    FROM inserted i
    WHERE i.MedicamentID IS NOT NULL 
            AND NOT EXISTS (
            SELECT 1
    FROM Lab13DB_1.dbo.MEDICAMENT m
    WHERE m.MedicamentID = i.MedicamentID
        )
    )
    BEGIN
        RAISERROR (N'Ошибка: Указанный MedicamentID не существует.', 16, 1);
        ROLLBACK TRANSACTION;
        RETURN;
    END;
END;
GO

IF OBJECT_ID(N'trg_UpdateLINEMEDICAMENT', N'TR') IS NOT NULL
    DROP TRIGGER trg_UpdateLINEMEDICAMENT;
GO

CREATE TRIGGER trg_UpdateLINEMEDICAMENT
ON LINEMEDICAMENT
AFTER UPDATE
AS
BEGIN
    SET NOCOUNT ON;

    -- Запрет изменения LineID
    IF UPDATE(LineID)
    BEGIN
        RAISERROR (N'Ошибка: Нельзя изменять LineID.', 16, 1);
        ROLLBACK TRANSACTION;
        RETURN;
    END;

    -- Проверка существования MedicamentID после обновления
    IF EXISTS (
        SELECT 1
    FROM inserted i
    WHERE UPDATE(MedicamentID) 
            AND i.MedicamentID IS NOT NULL 
            AND NOT EXISTS (
            SELECT 1
    FROM Lab13DB_1.dbo.MEDICAMENT m
    WHERE m.MedicamentID = i.MedicamentID
        )
    )
    BEGIN
        RAISERROR (N'Ошибка: Указанный MedicamentID не существует.', 16, 1);
        ROLLBACK TRANSACTION;
        RETURN;
    END;
END;
GO

USE Lab13DB_1;
GO

IF OBJECT_ID(N'trg_UpdateMEDICAMENT', N'TR') IS NOT NULL
    DROP TRIGGER trg_UpdateMEDICAMENT;
GO

CREATE TRIGGER trg_UpdateMEDICAMENT
ON MEDICAMENT
AFTER UPDATE
AS
BEGIN
    SET NOCOUNT ON;

    -- Запрет изменения MedicamentID
    IF UPDATE(MedicamentID)
    BEGIN
        RAISERROR ('Ошибка: Нельзя изменять MedicamentID.', 16, 1);
        ROLLBACK TRANSACTION;
        RETURN;
    END;
END;
GO

IF OBJECT_ID(N'trg_DeleteMEDICAMENT', N'TR') IS NOT NULL
    DROP TRIGGER trg_DeleteMEDICAMENT;
GO

CREATE TRIGGER trg_DeleteMEDICAMENT
ON MEDICAMENT
AFTER DELETE
AS
BEGIN
    SET NOCOUNT ON;

    -- Устанавливаем NULL для MedicamentID в LINEMEDICAMENT при удалении MEDICAMENT
    UPDATE Lab13DB_2.dbo.LINEMEDICAMENT
    SET MedicamentID = NULL
    WHERE MedicamentID IN (SELECT MedicamentID FROM deleted);
END;
GO

-- Создание представления 
USE Lab13DB_2;
GO

IF OBJECT_ID(N'LineWithMedicament', N'V') IS NOT NULL
    DROP VIEW LineWithMedicament;
GO

CREATE VIEW LineWithMedicament
AS
    SELECT
        l.LineID,
        l.OrderNumber,
        l.MedicamentID,
        m.MedicamentName,
        m.MedicamentGRLS,
        m.Form,
        m.StorageLife,
        m.Price AS UnitPrice,
        l."Count",
        l.LineCost
    FROM LINEMEDICAMENT l
        LEFT JOIN Lab13DB_1.dbo.MEDICAMENT m ON l.MedicamentID = m.MedicamentID;
GO

USE Lab13DB_2;
GO

PRINT N'1. Вставка данных в LINEMEDICAMENT.';
GO

INSERT INTO LINEMEDICAMENT (OrderNumber, MedicamentID, "Count", LineCost)
VALUES
    (1, 1, 2, 301.00),
    (1, 2, 1, 220.50),
    (2, 1, 5, 750.00);
GO

SELECT * FROM LineWithMedicament;
GO

PRINT N'2. Тестирование обновления данных.';
GO

UPDATE LINEMEDICAMENT
SET "Count" = 3, LineCost = 450.00
WHERE LineID = 1;
GO

SELECT * FROM LineWithMedicament;
GO

PRINT N'3. Тестирование запрета изменения LineID.';
GO

-- Попытка изменить LineID 
BEGIN TRY
    UPDATE LINEMEDICAMENT
    SET LineID = 10
    WHERE LineID = 1;
    PRINT N'Ошибка: изменение LineID должно быть запрещено!';
END TRY
BEGIN CATCH
    PRINT N'Ожидаемая ошибка: ' + ERROR_MESSAGE();
END CATCH
GO

PRINT N'4. Тестирование вставки с несуществующим MedicamentID.';
GO

-- Попытка вставки с несуществующим ID лекарства
BEGIN TRY
    INSERT INTO LINEMEDICAMENT (OrderNumber, MedicamentID, "Count", LineCost)
    VALUES (3, 999, 2, 500.00);  -- Несуществующий MedicamentID
    PRINT N'Ошибка: вставка с несуществующим MedicamentID должна быть запрещена!';
END TRY
BEGIN CATCH
    PRINT N'Ожидаемая ошибка: ' + ERROR_MESSAGE();
END CATCH
GO

PRINT N'5. Тестирование удаления лекарства.';
GO

SELECT * FROM LineWithMedicament;
GO

-- Удаление лекарства
DELETE FROM Lab13DB_1.dbo.MEDICAMENT
WHERE MedicamentID = 1;
GO

-- Проверяем, что в LINEMEDICAMENT установился NULL
SELECT N'После удаления лекарства:' AS Status, * FROM LineWithMedicament;
GO

PRINT N'6. Тестирование вставки строки без лекарства.';
GO

-- Вставка строки без указания лекарства
INSERT INTO LINEMEDICAMENT (OrderNumber, MedicamentID, "Count", LineCost)
VALUES (4, NULL, 10, 1000.00);
GO

SELECT N'Строка без лекарства:' AS Status, * FROM LineWithMedicament 
WHERE MedicamentID IS NULL;
GO

SELECT * FROM Lab13DB_1.dbo.MEDICAMENT;
SELECT * FROM LINEMEDICAMENT;
SELECT * FROM LineWithMedicament;
GO
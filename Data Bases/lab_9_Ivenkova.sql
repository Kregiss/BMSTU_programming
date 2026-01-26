USE master;
GO

IF DB_ID('Lab9DB') IS NOT NULL
BEGIN
  ALTER DATABASE Lab9DB SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
    DROP DATABASE Lab9DB;
END
GO

CREATE DATABASE Lab9DB
  ON PRIMARY
    ( NAME = Lab9DB_PrimaryData,
      FILENAME = 'E:\BaumDBdata\lab9\lab9db.mdf',
      SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
  LOG ON
    ( NAME = LogOne, FILENAME = 'E:\BaumDBdata\lab9\lab9dblog.ldf',
      SIZE = 5MB, MAXSIZE = 100MB, FILEGROWTH = 5MB)
GO

USE Lab9DB;
GO

-------------------------------------------------------------

CREATE TABLE MEDICAMENT (
	MedicamentID INT IDENTITY(1,1) PRIMARY KEY,
	MedicamentGRLS Nvarchar (15) UNIQUE NOT NULL,
	MedicamentName Nvarchar (150) NOT NULL,
	Form Tinyint NOT NULL CHECK (Form BETWEEN 0 and 20),
	StorageLife Smalldatetime NOT NULL,
	Price Smallmoney NOT NULL CHECK (Price >= 0)
);
GO

INSERT INTO MEDICAMENT (MedicamentGRLS, MedicamentName, Form, StorageLife, Price)
VALUES
    (N'GRLS001', N'Парацетамол', 11, '2026-01-01', 150.00),
    (N'GRLS002', N'Ибупрофен',   5,  '2027-05-01', 220.50);
GO

SELECT * FROM MEDICAMENT;
GO

CREATE TABLE MEDICAMENT_CERTIFICATE (
    MedicamentID INT PRIMARY KEY,
    CertificateNumber NVARCHAR(50) NOT NULL,
    IssueDate DATE NOT NULL,
    ExpiryDate DATE NOT NULL
    
    CONSTRAINT FK_MEDICAMENT_CERTIFICATE_MEDICAMENT 
        FOREIGN KEY (MedicamentID) 
        REFERENCES MEDICAMENT(MedicamentID)
        ON DELETE CASCADE
        ON UPDATE CASCADE,
    
    CONSTRAINT CHK_ExpiryDate CHECK (ExpiryDate > IssueDate)
);
GO

INSERT INTO MEDICAMENT_CERTIFICATE (MedicamentID, CertificateNumber, IssueDate, ExpiryDate)
VALUES
    (1, N'CERT-2023-001', '2023-01-15', '2028-01-15'),
    (2, N'CERT-2023-002', '2023-02-20', '2028-02-20');
GO

CREATE VIEW vwMedicamentWithCertificate
AS
SELECT 
    m.MedicamentID,
    m.MedicamentGRLS,
    m.MedicamentName,
    m.Form,
    m.StorageLife,
    m.Price,
    mc.CertificateNumber,
    mc.IssueDate,
    mc.ExpiryDate
FROM MEDICAMENT m
INNER JOIN MEDICAMENT_CERTIFICATE mc 
    ON m.MedicamentID = mc.MedicamentID;
GO

SELECT * FROM vwMedicamentWithCertificate;
GO

-------------------------------------------------------------
-- 1. Триггеры таблицы
-------------------------------------------------------------
CREATE TRIGGER trg_MEDICAMENT_INSERT
ON MEDICAMENT
AFTER INSERT
AS
BEGIN
    PRINT N'Триггер INSERT: препарат успешно добавлен в таблицу MEDICAMENT';
END;
GO

CREATE TRIGGER trg_MEDICAMENT_UPDATE
ON MEDICAMENT
AFTER UPDATE
AS
BEGIN
    IF UPDATE(Price) AND EXISTS (
        SELECT 1 
        FROM inserted i
        WHERE i.Price < 10000
    )
    BEGIN
        RAISERROR(N'Цена препарата не может быть ниже 10000', 16, 1);
        ROLLBACK TRANSACTION;
        RETURN;
    END

	IF UPDATE(MedicamentID)
    BEGIN
        RAISERROR(N'Нельзя изменять MedicamentID', 16, 1);
        ROLLBACK TRANSACTION;
        RETURN;
    END
        
    PRINT N'Триггер UPDATE: данные препарата успешно обновлены';
END;
GO

CREATE TRIGGER trg_MEDICAMENT_DELETE
ON MEDICAMENT
AFTER DELETE
AS
BEGIN
    PRINT N'Триггер DELETE: препарат успешно удален';
END;
GO

-------------------------------------------------------------
-- 2. Триггеры представления
-------------------------------------------------------------

CREATE TRIGGER trg_vwMedicamentWithCertificate_INSERT
ON vwMedicamentWithCertificate
INSTEAD OF INSERT
AS
BEGIN
    IF EXISTS (
		SELECT 1
		FROM inserted i
		JOIN MEDICAMENT m ON i.MedicamentGRLS = m.MedicamentGRLS
	)
	BEGIN 
		RAISERROR(N'MedicamentGRLS должен быть уникальным', 16, 1);
		ROLLBACK TRANSACTION;
		RETURN;
	END

	INSERT INTO MEDICAMENT (MedicamentGRLS, MedicamentName, Form, StorageLife, Price)
    SELECT MedicamentGRLS, MedicamentName, Form, StorageLife, Price
    FROM inserted;
        
    INSERT INTO MEDICAMENT_CERTIFICATE (MedicamentID, CertificateNumber, IssueDate, ExpiryDate)
    SELECT m.MedicamentID, CertificateNumber, IssueDate, ExpiryDate
    FROM inserted i
		INNER JOIN MEDICAMENT m ON i.MedicamentGRLS = m.MedicamentGRLS;
        
    PRINT N'Триггер INSERT для представления: данные добавлены через представление';
END;
GO

CREATE TRIGGER trg_vwMedicamentWithCertificate_UPDATE
ON vwMedicamentWithCertificate
INSTEAD OF UPDATE
AS
BEGIN
	IF UPDATE(MedicamentID)
    BEGIN
        RAISERROR(N'Обновление первичного ключа через это представление запрещено', 16, 1);
        RETURN;
    END
	   
    UPDATE m
    SET 
        m.MedicamentGRLS = i.MedicamentGRLS,
        m.MedicamentName = i.MedicamentName,
        m.Form = i.Form,
        m.StorageLife = i.StorageLife,
        m.Price = i.Price
    FROM MEDICAMENT m
    INNER JOIN inserted i ON m.MedicamentID = i.MedicamentID;
        
    UPDATE mc
    SET 
        mc.CertificateNumber = i.CertificateNumber,
        mc.IssueDate = i.IssueDate,
        mc.ExpiryDate = i.ExpiryDate
    FROM MEDICAMENT_CERTIFICATE mc
    INNER JOIN inserted i ON mc.MedicamentID = i.MedicamentID;
        
    PRINT N'Триггер UPDATE для представления: данные обновлены через представление';
    
END;
GO

CREATE TRIGGER trg_vwMedicamentWithCertificate_DELETE
ON vwMedicamentWithCertificate
INSTEAD OF DELETE
AS
BEGIN
    SET NOCOUNT ON;
    
	DELETE FROM MEDICAMENT
    WHERE MedicamentID IN (SELECT MedicamentID FROM deleted);
        
    PRINT N'Триггер DELETE для представления: данные удалены через представление';
    
END;
GO

/*
PRINT N'Тест 1';
INSERT INTO vwMedicamentWithCertificate 
(
    MedicamentGRLS, MedicamentName, Form, StorageLife, Price,
    CertificateNumber, IssueDate, ExpiryDate
)
VALUES 
(
    N'GRLS004', N'Анальгин', 5, '2027-06-30', 85.50,
    N'CERT-2023-004', '2023-04-01', '2028-04-01'
),
(
    N'GRLS005', N'Анальгин', 5, '2027-06-30', 85.50,
    N'CERT-2023-005', '2023-04-01', '2028-04-01'
);
GO
*/

/*
PRINT N'Тест 2';
UPDATE vwMedicamentWithCertificate 
SET Price = 11000 --Price = 160.00 < 10000
WHERE MedicamentID > 0;
GO
*/

/*
PRINT N'Тест 3';
DELETE FROM vwMedicamentWithCertificate 
WHERE MedicamentGRLS = N'GRLS001';

GO
*/

/*
PRINT N'Тест 4';
DELETE FROM vwMedicamentWithCertificate

GO
*/


SELECT * FROM MEDICAMENT;
SELECT * FROM MEDICAMENT_CERTIFICATE;
SELECT * FROM vwMedicamentWithCertificate;
GO

USE master;
GO

IF DB_ID('Lab7DB') IS NOT NULL
BEGIN
  ALTER DATABASE Lab7DB SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
    DROP DATABASE Lab7DB;
END
GO

CREATE DATABASE Lab7DB
  ON PRIMARY
    ( NAME = Lab7DB_PrimaryData,
      FILENAME = 'E:\BaumDBdata\lab7\lab7db.mdf',
      SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
  LOG ON
    ( NAME = LogOne, FILENAME = 'E:\BaumDBdata\lab7\lab7dblog.ldf',
      SIZE = 5MB, MAXSIZE = 100MB, FILEGROWTH = 5MB)
GO

USE Lab7DB;
GO

--------------------------------------------
-- 1. Представление на основе одной таблицы
--------------------------------------------
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

CREATE VIEW vwPharmacyInfo
AS
SELECT PharmacyName, PharmacyAddress, Mail
FROM PHARMACY;
GO

SELECT * FROM vwPharmacyInfo;
GO

------------------------------------------------------------
-- 2. Представление на основе полей обеих связанных таблиц
------------------------------------------------------------

CREATE TABLE MEDICAMENT (
  MedicamentID INT IDENTITY(1,1) PRIMARY KEY,
  MedicamentGRLS Nvarchar (15) NOT NULL,
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

CREATE TABLE LINEMEDICAMENT (
    LineID INT IDENTITY(1,1) PRIMARY KEY,
    OrderNumber INT NULL,
    MedicamentID INT NULL,
    "Count" SMALLINT NOT NULL CHECK ("Count" > 0),
    LineCost SMALLMONEY NOT NULL CHECK (LineCost >= 0)
);
GO

ALTER TABLE LINEMEDICAMENT
ADD CONSTRAINT FK_LINEMEDICAMENT_MEDICAMENT FOREIGN KEY (MedicamentID)
    REFERENCES MEDICAMENT(MedicamentID)
    ON DELETE CASCADE
    ON UPDATE CASCADE;
GO

INSERT INTO LINEMEDICAMENT (OrderNumber, MedicamentID, "Count", LineCost)
VALUES 
  (1, 1, 2, 301.50),
  (2, 1, 201, 50301.00),
  (3, 2, 20, 1200.90);
GO

SELECT * FROM LINEMEDICAMENT;
GO

CREATE VIEW vwMedicamentLinemedicament
AS
SELECT lm.LineID, lm.OrderNumber, lm.MedicamentID, lm."Count", lm.LineCost, m.MedicamentName, m.Form
FROM LINEMEDICAMENT lm
JOIN MEDICAMENT m 
    ON lm.MedicamentID = m.MedicamentID;
GO

SELECT * FROM vwMedicamentLinemedicament;
GO

-------------------------------------------------------
-- 3. Индекс с INCLUDE + пример ускоряющегося запроса
-------------------------------------------------------
CREATE TABLE CUSTOMER (
    CustomerID INT IDENTITY(1,1) PRIMARY KEY,
    FirstName NVARCHAR(25) NOT NULL,
    LastName NVARCHAR(25) NOT NULL,
    CustomerMail VARCHAR(254) NOT NULL,
    Phone CHAR(10) NULL,
  Age TINYINT NOT NULL CHECK (Age > 18),
  "Date" SMALLDATETIME NOT NULL DEFAULT GETDATE(),

);
GO

INSERT INTO CUSTOMER (FirstName, LastName, CustomerMail, Phone, Age)
VALUES 
    (N'Иван', N'Иванов', 'ivanivanov@mail.ru', '9157956346', 19),
    (N'Анна', N'Смирнова', 'annasmirnova@mail.ru', '9284376336', 20),
    (N'Дмитрий', N'Клиентов', 'dmitryclientov@mail.ru', '9281234336', 30);
GO

SELECT * FROM CUSTOMER;
GO

CREATE INDEX IX_Customer_Name
ON CUSTOMER(FirstName)
INCLUDE (Phone, Age);
GO

-- Пример запроса
SELECT 
    FirstName,
    Phone,
    Age
FROM CUSTOMER
WHERE FirstName LIKE N'А%';
GO


-------------------------------------
-- 4. Индексированное представление
-------------------------------------
CREATE VIEW dbo.vwMedLineMed
WITH SCHEMABINDING
AS
SELECT m.MedicamentID, COUNT_BIG(*) AS TotalRows, COUNT_BIG(lm.OrderNumber) AS TotalOrders  
FROM dbo.MEDICAMENT m
JOIN dbo.LINEMEDICAMENT lm 
    ON m.MedicamentID = lm.MedicamentID
GROUP BY m.MedicamentID;
GO

CREATE UNIQUE CLUSTERED INDEX IX_vwOrderCountByCustomer
ON dbo.vwMedLineMed (MedicamentID);
GO

SELECT * FROM dbo.vwMedLineMed;
GO

USE master;
GO

IF DB_ID('Lab6DB') IS NOT NULL
BEGIN
  ALTER DATABASE Lab6DB SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
    DROP DATABASE Lab6DB;
END
GO

CREATE DATABASE Lab6DB
  ON PRIMARY
    ( NAME = Lab6DB_PrimaryData,
      FILENAME = 'E:\BaumDBdata\lab6\lab6db.mdf',
      SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
  LOG ON
    ( NAME = LogOne, FILENAME = 'E:\BaumDBdata\lab6\lab6dblog.ldf',
      SIZE = 5MB, MAXSIZE = 100MB, FILEGROWTH = 5MB)
GO
USE Lab6DB;
--------------------------------------------------
-- 1. автоинкрементный PK
-- 2. CHECK, DEFAULT, функции
--------------------------------------------------
CREATE TABLE CUSTOMER (
    CustomerID INT IDENTITY(1,1) PRIMARY KEY,
    FirstName NVARCHAR(25) NOT NULL,
    LastName NVARCHAR(25) NOT NULL,
    CustomerMail VARCHAR(254) NOT NULL,
    Phone CHAR(10) NULL,
  Age TINYINT NOT NULL CHECK (Age > 18),
  "Date" SMALLDATETIME NOT NULL DEFAULT GETDATE()
);
GO

INSERT INTO CUSTOMER (FirstName, LastName, CustomerMail, Phone, Age)
VALUES 
    (N'Иван', N'Иванов', 'ivanivanov@mail.ru', '9157956346', 19),
    (N'Анна', N'Смирнова', 'annasmirnova@mail.ru', '9284376336', 20),
    (N'Тест', N'Клиентов', 'test@example.com', '0000000000', 30); 
GO

SELECT * FROM CUSTOMER;
GO

SELECT SCOPE_IDENTITY() AS [SCOPE_IDENTITY];
SELECT @@IDENTITY AS [@@IDENTITY];
SELECT IDENT_CURRENT('CUSTOMER') AS [IDENT_CURRENT];

--------------------------------------------------
-- 3. PHARMACY — GUID PK
--------------------------------------------------
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
--------------------------------------------------
-- 4. ORDER — SEQUENCE
--------------------------------------------------
CREATE SEQUENCE OrderSequence
    AS INT
    START WITH 1
    INCREMENT BY 1;
GO

CREATE TABLE "ORDER" (
    OrderNumber INT PRIMARY KEY DEFAULT NEXT VALUE FOR OrderSequence,
    "Date" SMALLDATETIME NOT NULL DEFAULT GETDATE(),
    OrderCost SMALLMONEY NOT NULL CHECK (OrderCost > 0),
    PayMethod TINYINT NOT NULL CHECK (PayMethod IN (0, 1, 2)),
    CustomerID INT NOT NULL,
    PharmacyID UNIQUEIDENTIFIER NOT NULL
);
GO

INSERT INTO "ORDER" (OrderCost, PayMethod, CustomerID, PharmacyID)
VALUES
    (525.25, 1, 1, (SELECT PharmacyID FROM PHARMACY WHERE PharmacyAddress = N'ул. Ленина, 1')),
    (840.50, 0, 1, (SELECT PharmacyID FROM PHARMACY WHERE PharmacyAddress = N'ул. Ленина, 1')),
    (315.75, 2, 2, (SELECT PharmacyID FROM PHARMACY WHERE PharmacyAddress = N'ул. Ленина, 1')),
    (1260.00, 1, 3, (SELECT PharmacyID FROM PHARMACY WHERE PharmacyAddress = N'ул. Ленина, 1')),
    (195.00, 0, 2, (SELECT PharmacyID FROM PHARMACY WHERE PharmacyAddress = N'ул. Ленина, 1'));
GO

SELECT * FROM "ORDER";
GO
--------------------------------------------------
-- 5. ограничение ссылочной целостности
--------------------------------------------------

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

-- LINEMEDICAMENT → MEDICAMENT
ALTER TABLE LINEMEDICAMENT
ADD CONSTRAINT FK_LINEMEDICAMENT_MEDICAMENT FOREIGN KEY (MedicamentID)
    REFERENCES MEDICAMENT(MedicamentID)
    ON DELETE SET NULL
    ON UPDATE SET NULL;
GO

INSERT INTO LINEMEDICAMENT (OrderNumber, MedicamentID, "Count", LineCost)
VALUES (1, 1, 2, 301.00);
GO

SELECT * FROM LINEMEDICAMENT;
GO

--------------------------------------------------

DELETE FROM MEDICAMENT WHERE Form = 11;

SELECT * FROM LINEMEDICAMENT;
GO
SELECT * FROM MEDICAMENT;
GO

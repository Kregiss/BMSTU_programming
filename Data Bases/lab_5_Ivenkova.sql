USE master;
GO
IF DB_ID(N'Lab5DB') IS NOT NULL
BEGIN
	ALTER DATABASE Lab5DB SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
    DROP DATABASE Lab5DB;
END
GO

CREATE DATABASE Lab5DB
  ON PRIMARY
    ( NAME = Lab5DB_PrimaryData,
      FILENAME = 'E:\BaumDBdata\lab5\lab5db.mdf',
      SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
  LOG ON
    ( NAME = LogOne, FILENAME = 'E:\BaumDBdata\lab5\lab5dblog.ldf',
      SIZE = 5MB, MAXSIZE = 100MB, FILEGROWTH = 5MB)
GO

USE Lab5DB;
GO

CREATE TABLE PHARMACY (
  PharmacyId INT PRIMARY KEY,
  PharmacyAddress NVARCHAR(200) NOT NULL,
  PharmacyName NVARCHAR(150) NOT NULL,
  Phone CHAR(10) NULL,
  Mail Varchar(254) NOT NULL
);
GO

INSERT INTO PHARMACY (PharmacyId, PharmacyAddress, PharmacyName, Phone, Mail)
VALUES (1, N'​Москва, ЖК Кварталы 21/19​2-й Грайвороновский проезд, 44 к3​1 этаж', N'Горздрав', '4996536277', 'feedback@gorzdrav.org'),
     (2, N'​Москва, Пресненская набережная, 12​1 этаж', N'36,6', '4957976336', 'feedback@366.ru'),  
     (3, N'Москва, ​ЖК Садовые кварталы, ​Улица Ефремова, 10 ст1', N'Магнит Аптека', '8002009002', 'apteka.online@magnit.ru')  
GO

SELECT * FROM PHARMACY;
GO

ALTER DATABASE Lab5DB ADD FILEGROUP Lab5DBLargeFileGroup;
GO

ALTER DATABASE Lab5DB
ADD FILE
  ( NAME = Lab5DB_LargeData,
    FILENAME = 'E:\BaumDBdata\lab5\Lab5DB_LargeData.ndf',
    SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
TO FILEGROUP Lab5DBLargeFileGroup;
GO

ALTER DATABASE Lab5DB MODIFY FILEGROUP Lab5DBLargeFileGroup DEFAULT;
GO


CREATE TABLE CUSTOMER (
  CustomerId INT PRIMARY KEY,
  FirstName NVARCHAR(25) NOT NULL,
  LastName NVARCHAR(25) NOT NULL,
  CustomerMail Varchar(254) NOT NULL,
  Phone CHAR(10) NULL
);
GO

INSERT INTO CUSTOMER(CustomerId, FirstName, LastName, CustomerMail, Phone)
VALUES (1, N'Иван', N'Иванов', 'ivanivanov@mail.ru', '9157956346'),
     (2, N'​Анна', N'Смирнова', 'annasmirnova@mail.ru', '9284376336')
GO

SELECT * FROM CUSTOMER;
GO

DROP TABLE CUSTOMER;
GO 

ALTER DATABASE Lab5DB
MODIFY FILEGROUP [PRIMARY] DEFAULT;
GO

ALTER DATABASE Lab5DB
REMOVE FILE Lab5DB_LargeData;
GO

ALTER DATABASE Lab5DB
REMOVE FILEGROUP Lab5DBLargeFileGroup;
GO

CREATE SCHEMA lab5schema AUTHORIZATION dbo;
GO

ALTER SCHEMA lab5schema TRANSFER dbo.PHARMACY;
GO

SELECT s.name AS SchemeName, t.name AS TableName FROM sys.tables t
  JOIN sys.schemas s ON t.schema_id = s.schema_id;
GO

ALTER SCHEMA dbo TRANSFER Lab5schema.PHARMACY;
GO

DROP SCHEMA lab5schema;
GO
USE master;
GO

IF DB_ID('Lab10DB') IS NOT NULL
BEGIN
  ALTER DATABASE Lab10DB SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
    DROP DATABASE Lab10DB;
END
GO

CREATE DATABASE Lab10DB
  ON PRIMARY
    ( NAME = Lab10DB_PrimaryData,
      FILENAME = 'E:\BaumDBdata\lab10\lab10db.mdf',
      SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
  LOG ON
    ( NAME = LogOne, FILENAME = 'E:\BaumDBdata\lab10\lab10dblog.ldf',
      SIZE = 5MB, MAXSIZE = 100MB, FILEGROWTH = 5MB)
GO

USE Lab10DB;
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

------------------------------------------------------------
---------------------READ UNCOMMITTED-----------------------
------------------------Dirty read--------------------------
/*
BEGIN TRANSACTION;

UPDATE PHARMACY
SET PharmacyName = N'Изменение READ UNCOMMITTED'
WHERE PharmacyName = N'Горздрав';
WAITFOR DELAY '00:00:05';
SELECT * FROM sys.dm_tran_locks;
ROLLBACK;
GO
*/

------------------------------------------------------------
---------------------READ COMMITTED-------------------------
------------------Non-repateable read-----------------------
/*
SET TRANSACTION ISOLATION LEVEL READ COMMITTED;
BEGIN TRANSACTION;

SELECT PharmacyName FROM PHARMACY
WHERE Mail = 'feedback@gorzdrav.org';
WAITFOR DELAY '00:00:05';
SELECT PharmacyName FROM PHARMACY
WHERE Mail = 'feedback@gorzdrav.org';

COMMIT;
GO 
*/
------------------------------------------------------------
---------------------REPEATABLE READ------------------------
----------------------Phantom read--------------------------
/*
SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;
BEGIN TRANSACTION;

SELECT * FROM PHARMACY;
WAITFOR DELAY '00:00:05';
SELECT * FROM PHARMACY;

COMMIT;
GO
*/
------------------------------------------------------------
---------------------SERIALIZABLE---------------------------
----------------------Phantom read--------------------------
/*
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;
BEGIN TRANSACTION;

SELECT * FROM PHARMACY;
WAITFOR DELAY '00:00:05';
SELECT * FROM PHARMACY;

COMMIT;
*/

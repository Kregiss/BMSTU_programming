USE master;
GO

IF DB_ID('Lab13DB_1') IS NOT NULL
BEGIN
  ALTER DATABASE Lab13DB_1 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
    DROP DATABASE Lab13DB_1;
END
GO

CREATE DATABASE Lab13DB_1
  ON PRIMARY
    ( NAME = Lab13DB_1_PrimaryData,
      FILENAME = 'E:\BaumDBdata\lab13_1\lab13_1db.mdf',
      SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
  LOG ON
    ( NAME = LogOne, FILENAME = 'E:\BaumDBdata\lab13_1\lab13_1dblog.ldf',
      SIZE = 5MB, MAXSIZE = 100MB, FILEGROWTH = 5MB)
GO

IF DB_ID('Lab13DB_2') IS NOT NULL
BEGIN
  ALTER DATABASE Lab13DB_2 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
    DROP DATABASE Lab13DB_2;
END
GO

CREATE DATABASE Lab13DB_2
  ON PRIMARY
    ( NAME = Lab13DB_2_PrimaryData,
      FILENAME = 'E:\BaumDBdata\lab13_2\lab13_2db.mdf',
      SIZE = 10, MAXSIZE = UNLIMITED, FILEGROWTH = 5%)
  LOG ON
    ( NAME = LogOne, FILENAME = 'E:\BaumDBdata\lab13_2\lab13_2dblog.ldf',
      SIZE = 5MB, MAXSIZE = 100MB, FILEGROWTH = 5MB)
GO

USE Lab13DB_1;
GO

CREATE TABLE Pharmacy1
(
    PharmacyId INT PRIMARY KEY,
    PharmacyAddress NVARCHAR(200) NOT NULL,
    PharmacyName NVARCHAR(150) NOT NULL,
    Phone CHAR(10) NULL,
    Mail VARCHAR(254) NOT NULL,
    CONSTRAINT CHK_Pharmacy1_ID CHECK (PharmacyId BETWEEN 1 AND 50)
);
GO


USE Lab13DB_2;
GO

CREATE TABLE Pharmacy2
(
    PharmacyId INT PRIMARY KEY,
    PharmacyAddress NVARCHAR(200) NOT NULL,
    PharmacyName NVARCHAR(150) NOT NULL,
    Phone CHAR(10) NULL,
    Mail VARCHAR(254) NOT NULL,
    CONSTRAINT CHK_Pharmacy2_ID CHECK (PharmacyId >= 51)
);
GO

USE Lab13DB_1;
GO

CREATE VIEW PharmacyView
AS
    -- Первая часть данных из базы Lab13DB_1
    SELECT PharmacyId, PharmacyAddress, PharmacyName, Phone, Mail
    FROM Lab13DB_1.dbo.Pharmacy1
    
    UNION ALL
    
    -- Вторая часть данных из базы Lab13DB_2
    SELECT PharmacyId, PharmacyAddress, PharmacyName, Phone, Mail
    FROM Lab13DB_2.dbo.Pharmacy2;
GO

INSERT INTO PharmacyView (PharmacyId, PharmacyAddress, PharmacyName, Phone, Mail)
VALUES 
    (1, N'Москва, ЖК Кварталы 21/19​2-й Грайвороновский проезд, 44 к3​1 этаж', N'Горздрав', '4996536277', 'feedback@gorzdrav.org'),
    (2, N'Москва, Пресненская набережная, 12​1 этаж', N'36,6', '4957976336', 'feedback@366.ru'),
    (3, N'Москва, ЖК Садовые кварталы, Улица Ефремова, 10 ст1', N'Магнит Аптека', '8002009002', 'apteka.online@magnit.ru'),
    (51, N'Санкт-Петербург, Невский проспект, 50', N'Аптека №1', '8123334455', 'apteka1@spb.ru'),
    (52, N'Екатеринбург, ул. Ленина, 25', N'Аптека здоровья', '8432223344', 'health@ekb.ru');
GO

SELECT * FROM PharmacyView ORDER BY PharmacyId;
GO

SELECT 'Lab13DB_1 - Part1' AS DatabasePart, * 
FROM Lab13DB_1.dbo.Pharmacy1;

SELECT 'Lab13DB_2 - Part2' AS DatabasePart, * 
FROM Lab13DB_2.dbo.Pharmacy2;
GO

-- 1. Удаление записи
DELETE FROM PharmacyView WHERE PharmacyName = '36,6';
GO

-- 2. Обновление записи
UPDATE PharmacyView 
SET PharmacyAddress = N'Москва, обновленный адрес, 100'
WHERE PharmacyId = 3;
GO

-- 3. Вставка новой записи
INSERT INTO PharmacyView (PharmacyId, PharmacyAddress, PharmacyName, Phone, Mail)
VALUES 
    (53, N'Казань, ул. Баумана, 15', N'Татарская аптека',  '8437778899', 'apteka@kazan.ru'),
    (4, N'Новосибирск, Красный проспект, 30', N'Сибирская аптека', '3834445566', 'apteka@nsk.ru');
GO

SELECT N'После операций' AS Status, * 
FROM PharmacyView 
ORDER BY PharmacyId;
GO

-- Проверка распределения данных по таблицам
SELECT N'Lab13DB_1 после операций' AS DatabasePart, * 
FROM Lab13DB_1.dbo.Pharmacy1;

SELECT N'Lab13DB_2 после операций' AS DatabasePart, * 
FROM Lab13DB_2.dbo.Pharmacy2;
GO

USE Lab10DB;

---------------------READ UNCOMMITTED------------------------------
/*
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
BEGIN TRANSACTION;

SELECT PharmacyName
FROM PHARMACY
WHERE Mail = 'feedback@gorzdrav.org';

COMMIT;
*/
---------------------READ COMMITTED------------------------------
/*
BEGIN TRANSACTION;
UPDATE PHARMACY
SET PharmacyName = N'Изменение READ COMMITTED'
WHERE PharmacyName = N'Горздрав';
SELECT * FROM sys.dm_tran_locks;
COMMIT;
*/
---------------------REPEATABLE READ------------------------------
/*
BEGIN TRANSACTION;
INSERT INTO PHARMACY (PharmacyAddress, PharmacyName, Phone, Mail)
VALUES (N'ул. Ленина, 213', N'Ещё одна Тестовая аптека', '9000000002', 'test2@apteka.ru')
SELECT * FROM PHARMACY
SELECT * FROM sys.dm_tran_locks;
COMMIT;
*/
---------------------SERIALIZABLE------------------------------
/*
BEGIN TRANSACTION;
INSERT INTO PHARMACY (PharmacyAddress, PharmacyName, Phone, Mail)
VALUES (N'Москва, Новая улица, 5', N'Новая аптека', '9999999999', 'new@apteka.ru');
SELECT * FROM PHARMACY
SELECT * FROM sys.dm_tran_locks;
COMMIT;
*/

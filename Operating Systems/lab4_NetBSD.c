// Подключение необходимых заголовков для драйверов и ядра
#include <ntddk.h>
#include <ntifs.h>
#include <ndk/exfuncs.h>
#include <ndk/ketypes.h>
#include <pseh/pseh2.h>
#include <ntstrsafe.h>

// Универсальный обработчик всех IRP-запросов
NTSTATUS
DefaultIrpHandler(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
    // Устанавливаем успешный статус запроса
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;
    
    // Завершаем обработку IRP
    IoCompleteRequest(Irp, IO_NO_INCREMENT);     
    return STATUS_SUCCESS;
}

// Функция выгрузки драйвера
VOID
NTAPI
DriverUnload(PDRIVER_OBJECT DriverObject)
{
    UNICODE_STRING symLink = RTL_CONSTANT_STRING(L"\\DosDevices\\lab4driver");

    // Удаляем символьную ссылку
    IoDeleteSymbolicLink(&symLink);

    // Удаляем устройство
    IoDeleteDevice(DriverObject->DeviceObject);

    // Печать сообщения об успешной выгрузке драйвера
    DbgPrint("(drivers/lab4/lab4.c:23) Driver unloaded successfully\n");
}

// Основная точка входа драйвера
NTSTATUS 
NTAPI
DriverEntry(PDRIVER_OBJECT DriverObject, PUNICODE_STRING RegistryPath)
{
    // Отключаем предупреждение о неиспользуемом параметре
    UNREFERENCED_PARAMETER(RegistryPath);

    NTSTATUS status;

    // Назначаем функцию выгрузки драйвера
    DriverObject->DriverUnload = (PDRIVER_UNLOAD)DriverUnload;
    
    // Определение имени устройства и символьной ссылки
    UNICODE_STRING deviceName = RTL_CONSTANT_STRING(L"\\Device\\lab4driver");
    UNICODE_STRING symLink = RTL_CONSTANT_STRING(L"\\DosDevices\\lab4driver");

    // Указатель на объект устройства
    PDEVICE_OBJECT g_DeviceObject = NULL;

    // Создание объекта устройства
    status = IoCreateDevice(DriverObject, 0, &deviceName, FILE_DEVICE_UNKNOWN, 0, FALSE, &g_DeviceObject);
    if (!NT_SUCCESS(status))
    {
        DbgPrint("IoCreateDevice failed: 0x%X\n", status);
        return status;
    }

    // Создание символьной ссылки на устройство
    status = IoCreateSymbolicLink(&symLink, &deviceName);
    if (!NT_SUCCESS(status))
    {
        IoDeleteDevice(g_DeviceObject);
        DbgPrint("IoCreateSymbolicLink failed: 0x%X\n", status);
        return status;
    }

    // Установка обработчика всех основных функций IRP
    for (int i = 0; i <= IRP_MJ_MAXIMUM_FUNCTION; i++)
    {
        DriverObject->MajorFunction[i] = (PDRIVER_DISPATCH)DefaultIrpHandler;
    }

    // Работа с виртуальной памятью

    // Размер резервируемой области памяти (10 страниц)
    SIZE_T size = PAGE_SIZE * 10;

    // Размер коммитируемой области (5 страниц)
    SIZE_T committedSize = PAGE_SIZE * 5;

    PVOID addr = NULL;
    volatile SIZE_T access;
    PHARDWARE_PTE_X86 pPte = NULL;

    // Резервирование виртуальной памяти без выделения физических страниц
    status = ZwAllocateVirtualMemory(NtCurrentProcess(), &addr, 0, &size, MEM_RESERVE, PAGE_READWRITE);
    if (!NT_SUCCESS(status))
    {
        DbgPrint("MEM_RESERVE failed: 0x%X\n", status);
        return status;
    }

    // Коммит (выделение физических страниц для первых 5 страниц)
    size = committedSize;
    status = ZwAllocateVirtualMemory(NtCurrentProcess(), &addr, 0, &size, MEM_COMMIT, PAGE_READWRITE);
    if (!NT_SUCCESS(status))
    {
        DbgPrint("MEM_COMMIT failed: 0x%X\n", status);
        ZwFreeVirtualMemory(NtCurrentProcess(), &addr, &size, MEM_RELEASE);
        return status;
    }

    // Получение указателя на PTE для выделенного диапазона виртуальной памяти
    pPte = (PHARDWARE_PTE_X86)(0xC0000000 + ((ULONG_PTR)addr >> 10));

    // Перебор всех 10 страниц и вывод их состояния
    for (SIZE_T i = 0; i < 10; i++)
    {
        if (i < 5)
        {
            // Принудительное чтение для создания записи в таблице страниц (если требуется)
            access = *(volatile SIZE_T *)((PUCHAR)addr + i * PAGE_SIZE);
            DbgPrint("Page %Iu access: %Ix\n", i, access);
        }
        else
        {
            DbgPrint("Page %Iu\n", i);
        }

        // Вывод информации о каждой странице
        DbgPrint("valid - %s\n", pPte->Valid ? "true" : "false");
        DbgPrint("dirty - %s\n", pPte->Dirty ? "true" : "false");
        DbgPrint("physical address - 0x%08LX\n\n", pPte->PageFrameNumber << 12);

        // Переход к следующей записи PTE
        pPte++;
    }

    // Декоммит памяти (освобождение физических страниц)
    size = committedSize;
    ZwFreeVirtualMemory(NtCurrentProcess(), &addr, &size, MEM_DECOMMIT);

    // Освобождение зарезервированной виртуальной памяти
    size = 0;
    ZwFreeVirtualMemory(NtCurrentProcess(), &addr, &size, MEM_RELEASE);

    // Сообщение о завершении теста памяти
    DbgPrint("Memory test complete.\n");

    return STATUS_SUCCESS;
}

#include <ntddk.h>
#include <ntifs.h>
#include <ndk/exfuncs.h>
#include <ndk/ketypes.h>
#include <pseh/pseh2.h>
#include <ntstrsafe.h>

NTSTATUS DriverEntry(PDRIVER_OBJECT DriverObject, PUNICODE_STRING RegistryPath);
VOID DriverUnload(PDRIVER_OBJECT DriverObject);

NTSTATUS DriverEntry(PDRIVER_OBJECT DriverObject, PUNICODE_STRING RegistryPath) {
    UNREFERENCED_PARAMETER(RegistryPath);
    NTSTATUS status;
    ULONG bufferSize = 1024 * 1024;  // Начальный размер буфера (1 MB)
    SYSTEM_PROCESS_INFORMATION *buffer = NULL;
    ULONG len;

    // Выделяем память для буфера
    buffer = (SYSTEM_PROCESS_INFORMATION *)ExAllocatePool(PagedPool, bufferSize);
    if (buffer == NULL) {
        DbgPrint("(drivers/lab3/lab3.c:36) DriverEntry: Memory allocation failed\n");
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    // Запрашиваем информацию о процессах
    status = ZwQuerySystemInformation(SystemProcessInformation, buffer, bufferSize, &len);
    if (status == STATUS_INFO_LENGTH_MISMATCH) {
        DbgPrint("(drivers/lab3/lab3.c:43) DriverEntry: Buffer too small, adjusting size\n");
        bufferSize = len;  // Увеличиваем размер буфера
        ExFreePool(buffer);
        buffer = (SYSTEM_PROCESS_INFORMATION *)ExAllocatePool(PagedPool, bufferSize);
        if (buffer == NULL) {
            DbgPrint("(drivers/lab3/lab3.c:49) DriverEntry: Memory reallocation failed\n");
            return STATUS_INSUFFICIENT_RESOURCES;
        }

        // Повторный запрос с новым размером буфера
        status = ZwQuerySystemInformation(SystemProcessInformation, buffer, bufferSize, &len);
    }

    if (!NT_SUCCESS(status)) {
        DbgPrint("(drivers/lab3/lab3.c:56) DriverEntry: ZwQuerySystemInformation failed, status = 0x%x\n", status);
        ExFreePool(buffer);
        return status;
    }

    // Если статус успешен, выводим информацию о процессах
    DbgPrint("(drivers/lab3/lab3.c:64) Process list:\n");

    SYSTEM_PROCESS_INFORMATION *entry = buffer;
    while (entry) {
        // Выводим информацию о процессе в требуемом формате
        if (entry->ImageName.Buffer != NULL && entry->ImageName.Length > 0) {
            DbgPrint("%u: \t%wZ  \t(Parent PID: %u)\n", 
                entry->UniqueProcessId, 
                &entry->ImageName,                     // Используем UniqueProcessId
                entry->InheritedFromUniqueProcessId    // Используем InheritedFromUniqueProcessId
                );
        } else {
            DbgPrint("%u: \t(null)\n", entry->UniqueProcessId);
        }

        if (entry->NextEntryOffset == 0) {
            break;
        }
        entry = (SYSTEM_PROCESS_INFORMATION *)((PUCHAR)entry + entry->NextEntryOffset);
    }

    ExFreePool(buffer);
    return STATUS_SUCCESS;
}

VOID DriverUnload(PDRIVER_OBJECT DriverObject) {
    UNREFERENCED_PARAMETER(DriverObject);
    DbgPrint("(drivers/lab3/lab3.c:84) Driver unloaded successfully\n");
}

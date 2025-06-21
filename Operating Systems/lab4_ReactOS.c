#include <sys/module.h>      // Подключение интерфейса для работы с модулями ядра
#include <sys/kernel.h>      // Базовые функции ядра
#include <uvm/uvm_extern.h>  // Интерфейсы управления виртуальной памятью
#include <uvm/uvm_pmap.h>    // Работа с отображением виртуальной памяти в физическую
#include <uvm/uvm.h>         // Основные структуры UVM
#include <sys/systm.h>       // Базовые функции системы (например, printf)
#include <sys/param.h>       // Основные параметры системы
#include <sys/queue.h>       // Макросы для работы со списками (TAILQ и др.)

// Объявление модуля ядра
MODULE(MODULE_CLASS_MISC, lab4, NULL);

// Количество страниц виртуальной памяти для выделения
#define NUM_PAGES 10

// Указатель на список физических страниц
static struct vm_page *pglist = NULL;

// Функция печати информации о странице
static void print_page_info(vaddr_t va, int page_num) {
    paddr_t pa = 0;
    // Извлечение физического адреса по виртуальному адресу
    bool valid = pmap_extract(pmap_kernel(), va, &pa);
    bool used = false;
    bool modified = false;

    if (valid) {
        // Получение структуры vm_page по физическому адресу
        struct vm_page *pg = PHYS_TO_VM_PAGE(pa);
        if (pg != NULL) {
            // Проверка, использовалась ли страница
            used = pmap_is_referenced(pg);
            // Проверка, модифицировалась ли страница
            modified = pmap_is_modified(pg);
        }
    }

    // Вывод информации о странице
    printf("Page - %d\n", page_num);
    printf("Valid - %s\n", valid ? "true" : "false");
    printf("Used - %s\n", used ? "true" : "false");
    printf("Modified - %s\n", modified ? "true" : "false");
    printf("Physical address - 0x%08lx\n\n", (unsigned long)pa);
}

// Основная функция обработки команд модуля
static int lab4_modcmd(modcmd_t cmd, void *arg) {
    vaddr_t addr;
    vsize_t size = NUM_PAGES * PAGE_SIZE; // Размер памяти для выделения (10 страниц)

    if (cmd == MODULE_CMD_INIT) { // Инициализация модуля
        // Выделение виртуального адресного пространства без привязки к физическим страницам
        addr = uvm_km_alloc(kernel_map, size, PAGE_SIZE, UVM_KMF_VAONLY);
        if (addr == 0) {
            printf("lab4: virtual memory allocation failed\n");
            return ENOMEM; // Ошибка нехватки памяти
        }

        struct pglist mlist;
        TAILQ_INIT(&mlist); // Инициализация пустого списка страниц

        // Выделение 5 физических страниц
        int r = uvm_pglistalloc(5 * PAGE_SIZE, 0, 0xffffffff, 0, 0, &mlist, 5, 0);
        if (r != 0) {
            // Ошибка при выделении физической памяти, освобождаем виртуальное пространство
            uvm_km_free(kernel_map, addr, size, 0);
            printf("lab4: physical memory allocation failed\n");
            return ENOMEM;
        }

        pglist = TAILQ_FIRST(&mlist); // Сохраняем указатель на первую страницу списка

        // Привязка первых 5 виртуальных страниц к выделенным физическим страницам
        struct vm_page *pg = TAILQ_FIRST(&mlist);
        for (int i = 0; i < 5 && pg != NULL; i++) {
            paddr_t pa = VM_PAGE_TO_PHYS(pg); // Получаем физический адрес
            pmap_kenter_pa(addr + i * PAGE_SIZE, pa, VM_PROT_READ | VM_PROT_WRITE, 0); // Связываем виртуальный и физический адрес
            pg = TAILQ_NEXT(pg, pageq.queue); // Переходим к следующей странице в списке
        }

        // Обновление отображения памяти
        pmap_update(pmap_kernel());

        // Печать состояния страниц до освобождения памяти
        printf("Before free:\n");
        for (int i = 0; i < 10; i++) {
            print_page_info(addr + i * PAGE_SIZE, i + 1);
        }

        // Удаление отображения виртуальных адресов
        pmap_kremove(addr, size);
        pmap_update(pmap_kernel());

        // Освобождение выделенной физической памяти
        if (pglist)
            uvm_pglistfree(&mlist);

        // Освобождение виртуального адресного пространства
        uvm_km_free(kernel_map, addr, size, 0);

        return 0; // Успешное завершение
    }

    if (cmd == MODULE_CMD_FINI) { // Выгрузка модуля
        printf("lab4: module unloaded\n");
        return 0;
    }

    return ENOTTY; // Команда не поддерживается
}

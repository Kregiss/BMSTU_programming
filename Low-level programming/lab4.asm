assume cs:code, ds:data

data segment
    var1 db 10             ; 1 байт
    var2 dw 1000h          ; 2 байта (слово)
    var3 dd 12345678h      ; 4 байта (двойное слово)
    var4 db 12             ; 1 байт
    var5 db 13             ; 1 байт
    var6 db 14             ; 1 байт
    arr db 5 dup(0)        ; массив из 5 байтов
data ends

; Описать макрос VOLUME p1, p2, p3, p4, p5, p6, p7, который подсчитывает и
; печатает общее количество байтов, занимаемое переданными параметрами в
; сегменте данных. Параметры — имена переменных (или массивов, описанных единичным 
; оператором dup) или константы. Учесть, что константы в памяти не хранятся.

code segment
org 100h

VOLUME macro p1, p2, p3, p4, p5, p6, p7  
    local totalSize, checkNext
    totalSize = 0

    check_size macro param
        ifnb <param>             ; Проверяем, что параметр не пустой
            ifdef param
                totalSize = totalSize + size param
	    else
                mov dx, offset undef_error_msg
                mov ah, 9
                int 21h
            endif
        endif
    endm

    check_size p1
    check_size p2
    check_size p3
    check_size p4
    check_size p5
    check_size p6
    check_size p7

    mov ax, totalSize
    call print_number
endm

undef_error_msg db '         Error: Undefined variable used.', 0Dh, 0Ah, '$'

print_number proc
    push ax
    push dx
    xor cx, cx
    mov bx, 10        

convert_loop:
    xor dx, dx
    div bx
    push dx
    inc cx
    or ax, ax
    jnz convert_loop

print_digits:
    pop dx
    add dl, '0'
    mov ah, 02h
    int 21h
    loop print_digits

    pop dx
    pop ax
    ret
print_number endp

main:
    mov ax, data
    mov ds, ax

    VOLUME var1, var2, var3, arr, var4, var5, var6

    ; Завершение программы
    mov ah, 4Ch
    int 21h

code ends
end main

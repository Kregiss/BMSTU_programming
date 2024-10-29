assume cs:code, ds:data

data segment
    input_str db 255, 254 dup(0)      ; строка для ввода
    search_char db 0                  ; символ для поиска
    not_found_msg db "  Character not found in the string$", '$'
    found_msg db "  ", '$'
data ends

code segment

; Процедура для ввода строки с клавиатуры
input_string proc
    push bp
    mov bp, sp
    mov dx, [bp+4]
    xor ax, ax
    mov ah, 0Ah
    int 21h
    mov dx, [bp+4]
    inc dx
    mov si, dx
    mov cx, [si]
    xor ch, ch
    add si, cx
    mov byte ptr [si+1], '$'
    inc dx
    pop bp
    ret
input_string endp

; Процедура для вывода строки
print_string proc
    push bp
    mov bp, sp
    mov dx, [bp+4]
    add dx, 2
    xor ax, ax
    mov ah, 09h
    int 21h
    pop bp
    ret
print_string endp

; последнее вхождение символа в строке
strrchr proc
    push bp
    mov bp, sp
    mov si, [bp+4]     ; указатель на строку
    mov al, [bp+6]     ; символ для поиска
    mov cx, 0          ; счетчик длины строки
    mov bx, -1         ; индекс последнего вхождения символа (-1, если не найден)

    ; Поиск конца строки
    find_end:
        push cx
        add si, cx
        mov dl, [si]
        sub si, cx
        pop cx
        cmp dl, 0
        je search_done
        cmp dl, al
        jne next_char
        mov bx, cx      
    next_char:
        inc cx
        jmp find_end

    search_done:
        mov ax, bx     
        pop bp
        ret
strrchr endp

print_offset proc
    push bp
    mov bp, sp
    mov ax, [bp+4]
    or ax, ax
    jge print_positive

    ; если символ не найден
    mov dx, offset not_found_msg
    call print_string
    jmp end_print

print_positive:
    ; Преобразование в десятичное представление
    xor cx, cx
    mov bx, 10
convert_loop:
    xor dx, dx
    div bx
    push dx
    inc cx
    or ax, ax
    jnz convert_loop

; Вывод числа на экран
print_digits:
    pop dx
    add dl, '0'
    mov ah, 02h
    int 21h
    loop print_digits

end_print:
    pop bp
    ret
print_offset endp

endl proc
  mov ah, 02h
  mov dl, 0Ah
  int 21h
  ret
endl endp

main:
    mov ax, data
    mov ds, ax

    ; Ввод строки
    push offset input_str
    call input_string
    call endl

    ; Ввод символа для поиска
    mov ah, 1  
    int 21h
    mov search_char, al 
    call endl

    ; Вызов strrchr
    mov al, search_char              
    push ax                           
    push offset input_str+2           
    call strrchr
    mov bx, ax                        

    cmp bx, -1
    je character_not_found

    ; Если найден, выводим сообщение и смещение
    push offset found_msg
    call print_string
    push bx
    call print_offset
    jmp exit_program

character_not_found:
    push offset not_found_msg
    call print_string

exit_program:
    mov ah, 4Ch
    int 21h

code ends
end main

data segment
    arr dw 5, 3, 8, 1, 10, 20   
    len dw 6                  ; Длина массива
    output_buffer db '00000$', 0  
data ends

code segment
assume cs:code, ds:data

start:
    mov ax, data              ; Каждому элементу массива, начиная со второго, присвоить значение максимального элемента из числа ему предшествующих и его самого.
    mov ds, ax

    mov si, 0                 
    mov ax, [arr]             
    mov bx, ax                ; максимум

    mov cx, len               
    dec cx                    

process_array:
    add si, 2                 
    mov ax, [arr + si]        
    cmp ax, bx                
    jle skip_update           
    mov bx, ax                

skip_update:
    mov [arr + si], bx        
    loop process_array        

    mov si, 0                 
    mov cx, len               

print_array:
    mov ax, [arr + si]        
    call print_number         
    add si, 2                 
    loop print_array          

    mov ah, 4Ch
    int 21h

print_number:
    mov di, offset output_buffer + 5  

convert_to_string:
    xor dx, dx
    mov bx, 10               
    div bx
    add dl, '0'              
    dec di
    mov [di], dl
    test ax, ax
    jnz convert_to_string

    mov ah, 09h              
    mov dx, di
    int 21h

    ; Выводим пробел между числами
    mov ah, 02h
    mov dl, ' '
    int 21h

    ret

code ends
end start

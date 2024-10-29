assume CS:code, DS:data

data segment
    a dw 5 
    b dw 3 
    c dw 4
    d dw 123
    res dw ?            
    dec_output_buffer db "00000$", 0
    hex_output_buffer db "00000$", 0
data ends

code segment
start:
    mov ax, data       
    mov ds, ax

    mov ax, a          ; c - (a + b)/2 + d
    add ax, b          

    mov bx, 2        
    cwd  
    div bx             

    mov bx, c          
    sub bx, ax         

    add bx, d          
    mov res, bx        

    mov ax, res
    mov di, offset dec_output_buffer + 5  

convert_to_dec:
    xor dx, dx
    mov bx, 10
    div bx
    add dl, '0'
    dec di
    mov [di], dl
    test ax, ax
    jnz convert_to_dec

    mov ah, 09h
    mov dx, di
    int 21h

newline:
    mov ah, 2
    mov dl, 10
    int 21h

    mov ax, res
    mov di, offset hex_output_buffer + 5

convert_to_hex:
    xor dx, dx
    mov bx, 16
    div bx
    cmp dl, 10
    jl not_letter
    add dl, 'A' - 10
    jmp write_hex_digit

not_letter:
    add dl, '0'

write_hex_digit:
    dec di
    mov [di], dl
    test ax, ax
    jnz convert_to_hex

    mov ah, 09h
    mov dx, di
    int 21h

    ; Завершение программы
    mov ah, 4Ch
    int 21h

code ends
end start

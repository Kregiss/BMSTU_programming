assume CS:code, DS:data

data segment
    a dw 5 
    b dw 3 
    c dw 4
    d dw 2
data ends

code segment
start:
    mov ax, data      ; c - (a + b)/2 + d
    mov ds, ax

    mov ax, a          
    add ax, b             
       
    mov bx, 2          
    div bx             

    mov bx, c          
    sub bx, ax         

    add bx, d         

    ; Завершение программы
    mov ah, 4Ch        
    int 21h
code ends
end start

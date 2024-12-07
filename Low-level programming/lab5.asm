assume cs: code, ds: data

data segment
    newline db 0Ah, "$" 
    myStr db 100, 99 dup ('$')
    str1 db 100, 99 dup ('$')
    max_len dw 16
    num_a db 100, 99 dup (0) 
    num_b db 100, 99 dup (0) 
    num_c db 100, 99 dup (0) ; результат
    system db 10
    cmpres db 0

    error_wrong_symbol db 100, " error: it's not a numeric symbol $"
    sum_message db "Sum: $", 0
    prod_message db "Product: $", 0
data ends

code segment

initds macro
    mov ax, data
    mov ds, ax
endm

endprogram macro
    mov ah, 4ch
    int 21h
endm

print macro myStr
    push ax
    mov ah, 09h
    lea dx, myStr
    add dx, 2 
    int 21h
    pop ax
endm

println macro myStr
    print myStr
    push ax
    mov ah, 09h
    lea dx, newline
    int 21h
    pop ax
endm

printchar macro char
    push ax
    push dx
    mov ah, 2
    mov dl, char
    int 21h
    pop dx
    pop ax
endm

printdig macro digit
    push dx
    mov dh, digit
    add dh, '0'
    printchar dh
    sub dh, '0'
    pop dx
endm

error macro message
    println message
    endprogram
endm

error_symbol macro message, symbol
    print message
    printchar symbol
    endprogram
endm

scanstr macro myStr
    mov dx, offset myStr
    xor ax, ax
    mov ah, 0Ah
    int 21h
    mov si, dx
    xor bh, bh
    mov bl, [si+1]
    mov ch, '$'
    add bx, 2
    mov [si+bx], ch
    mov dx, offset newline
    mov ah, 09h
    int 21h
endm

to_string macro num, output_string
    push ax
    push bx
    push cx
    push di
    mov ax, num
    mov di, 4 
    mov cx, 5 
    MOV BL, system
    mov output_string[5], 10
    mov output_string[6], 13
    goto:
        DIV BL 
        mov output_string[di], ah
        add output_string[di],"0"
        mov ah,0
        sub di,1 
    loop goto
    
    pop di
    pop cx
    pop bx
    pop ax
endm

ifless macro a, b, endmark
    cmp a, b
    jge endmark
endm

ifequal macro a, b, endmark
    cmp a, b
    je endmark
endm

ifnotspace macro symbol, endmark
    push ax
    push bx
    mov ah, ' '
    mov bh, symbol
    cmp bh, ah
    pop bx
    pop ax
    je endmark
endm

ifnotend macro symbol, endmark
    push ax
    push bx
    mov ah, '$'
    mov bh, symbol
    cmp bh, ah
    pop bx
    pop ax
    je endmark
endm

ifflag macro flagname, endmark
    push ax
    push bx
    xor ax, ax
    mov bx, flagname
    cmp bx, ax
    pop bx
    pop ax
    je endmark
endm

ifnotnumber macro symbol, endmark
    push ax
    mov al, '/'
    mov ah, '0'
    add ah, system
    ifless symbol, ah, _&endmark
    ifless al, symbol, _&endmark
        pop ax
        jmp endmark
    _&endmark&:   
    pop ax
endm

ifnotminus macro symbol, endmark
    push ax
    push bx
    mov ah, '-'
    mov bh, symbol
    cmp bh, ah
    pop bx
    pop ax
    je endmark
endm

ifminus macro symbol, endmark
    push ax
    push bx
    mov ah, '-'
    mov bh, symbol
    cmp bh, ah
    pop bx
    pop ax
    jne endmark
endm

set_true macro flagname
    mov flagname, 1
endm

setfalse macro flagname
    mov flagname, 0
endm

mv_symbol macro s1, s2
    push ax
    mov ah, s2
    mov s1, ah
    pop ax
endm

strlen macro myStr, reg
    xor reg&x, reg&x
    mov reg&l, myStr[1]
endm

to_hex proc
    mov cl, 60h
    ifless cl, ch, tohexendif
        sub ch, 'a'
        add ch, ':'
    tohexendif:
    ret
to_hex endp

from_hex proc
    mov cl, '9'
    ifless cl, ch, fromhexendif
        sub ch, ':'
        add ch, 'a'
    fromhexendif:
    ret
from_hex endp

num_to_string proc
    mov bp, sp
    mov si, [bp + 2]
    mov ax, max_len
    xor di, di 
    add si, max_len
    mov bl, [si]
    cmp bx, 0
    je plus
        printchar '-'
        jmp endsign
    plus:
        printchar '+'
    endsign:
    sub si, max_len

    mov bx, 2
    loop_numtostring:
        mov ch, [si]
        add ch, '0'
        call from_hex
        mov myStr[bx], ch

        inc si
        inc di
        inc bx
        ifless di, ax, break_numtostring
            jmp loop_numtostring
        break_numtostring:
    ret
num_to_string endp

printnum macro num
    mov dx, offset num
    push dx
    call num_to_string
    println myStr
endm

print_sum_message macro
    mov dx, offset sum_message
    mov ah, 09h
    int 21h
endm

print_prod_message macro
    mov dx, offset prod_message
    mov ah, 09h
    int 21h
endm

to_num proc
    mov bp, sp
    mov di, [bp + 2]
    strlen myStr, a
    mov bx, max_len
    sub bx, ax 
    add ax, 2
    mov si, 2 
    xor dx, dx
    mov [di], dx 
    loop_tonum:
        mov ch, myStr[si]
        
        call to_hex
        
        ifnotnumber ch, ok_it_is_number
        ifnotminus ch, minus_case
            error_symbol error_wrong_symbol, ch
        ok_it_is_number:


        jmp number_case
        minus_case:
            push ax
            add di, max_len
            mov ax, [di]
            not ax
            mov [di], ax
            sub di, max_len
            pop ax
            jmp endcase
        number_case:
            sub ch, '0'
            mov [di + bx], ch
        endcase:

        inc si
        inc bx
        ifless si, ax, break_tonum
            jmp loop_tonum
        break_tonum:
    ret
to_num endp

inv_sign macro num
    push di
    push ax
    mov di, max_len
    mov al, num[di]
    not al
    mov num[di], al
    pop ax
    pop di
endm

sw_nums proc
    push si
    push ax
    push bx
    mov si, max_len 
    dec si
    loop_swap:
        mov al, num_a[si]
        mov bl, num_b[si]
        mov num_a[si], bl
        mov num_b[si], al

        dec si
        cmp si, 0
        je break_swap
        jmp loop_swap
    break_swap:    
    pop bx
    pop ax
    pop si
    ret
sw_nums endp

compare_nums proc
    push di
    push ax
    push bx    
    xor ax, ax
    xor bx, bx
    xor si, si
    mov di, max_len
    mov al, num_a[di]
    mov bl, num_b[di]

    cmp ax, bx
    je loop_comp
    jl sign_less
        mov cmpres, 2        
        jmp endcompare_nums
    sign_less:
        mov cmpres, 1
        jmp endcompare_nums
    
    loop_comp:
        mov al, num_a[si]
        mov bl, num_b[si]
        cmp ax, bx
        je cmp_equal
        jl equal_less
            mov cmpres, 1
            jmp break_comp
        equal_less:
            mov cmpres, 2
            jmp break_comp
        cmp_equal:

        inc si
        cmp si, max_len
        jge break_comp
        jmp loop_comp
    break_comp:

    endcompare_nums:
    pop bx
    pop ax
    pop di
    ret
compare_nums endp

scan_num macro num
    scanstr myStr
    mov dx, offset num
    push dx
    call to_num
endm

calc_sum proc
    mov di, max_len
    mov al, num_a[di]
    mov bl, num_b[di]
    cmp al, bl
    je skipdiff
        inv_sign num_b
        call calc_diff
        ret
    skipdiff:
    call compare_nums
    cmp cmpres, 2
    jne not_swap_
        call sw_nums
        inv_sign num_c
    not_swap_: 

    mov di, max_len
    mov al, num_a[di]
    cmp al, 0
    je invert_sign_in_diff_
        inv_sign num_c
    invert_sign_in_diff_: 

    mov si, max_len
    sub si, 1
    loop_sum:
        xor cx, cx
        mov ah, num_a[si]
        mov bh, num_b[si]
        mov ch, num_c[si]
        add ch, ah
        add ch, bh
        mov cl, system
        dec cl
        ifless cl, ch, sum_overflow
            sub ch, system
            mov cl, 1
            mov num_c[si - 1], cl
        sum_overflow:

        mov num_c[si], ch

        dec si
        cmp si, 0
        jl break_sum
        jmp loop_sum
    break_sum:
    ret
calc_sum endp


calc_diff proc
    mov di, max_len
    mov al, num_a[di]
    mov bl, num_b[di]
    cmp al, bl
    je skipsum
        inv_sign num_b
        call calc_sum
        ret
    skipsum:
    call compare_nums
    cmp cmpres, 2
    jne not_swap
        call sw_nums
        inv_sign num_c
    not_swap:

    mov di, max_len
    mov al, num_a[di]
    cmp al, 0
    je invert_sign_in_diff
        inv_sign num_c
    invert_sign_in_diff:

    mov si, max_len
    sub si, 1
    xor dh, dh 

    loop_diff:
        xor cx, cx
        mov ah, num_a[si]
        mov bh, num_b[si]
        add ch, ah
        sub ch, bh
        sub ch, dh
        xor cl, cl
        xor dh, dh
        ifless ch, cl, diff_overflow
            add ch, system
            mov dh, 1
            
        diff_overflow:

        mov num_c[si], ch

        dec si
        cmp si, 0
        jl break_diff
        jmp loop_diff
    break_diff:
    ret
calc_diff endp

calc_prod proc
    mov di, max_len
    sub di, 1
    xor bx, bx
    loop_sumprod:
        mov si, max_len
        sub si, 1
        loop_prod:
            xor ax, ax
            xor cx, cx
            xor dx, dx
            mov al, num_a[si]
            mov dl, num_b[di]
            mul dx
            mov cl, system
            div cl
            sub si, bx
            add num_c[si - 1], al
            add num_c[si], ah
            add si, bx

            dec si
            cmp si, 0
            jl break_prod
            jmp loop_prod
        break_prod:

        inc bx
        dec di
        cmp di, 0
        jl break_sumprod
        jmp loop_sumprod
    break_sumprod:

    mov di, max_len
    sub di, 1
    loop_fix:
        xor ax, ax
        mov cl, system
        mov al, num_c[di]
        div cl
        add num_c[di - 1], al
        mov num_c[di], ah

        dec di
        cmp di, 0
        jl break_fix
        jmp loop_fix
    break_fix:
    mov di, max_len
    push ax
    push bx
    mov al, num_a[di]
    mov bl, num_b[di]
    xor al, bl
    mov num_c[di], al
    pop bx
    pop ax
    ret
calc_prod endp

start:
    initds

    scan_num num_a
    scan_num num_b
    xor dx, dx
    mov num_c[0], dh 

    ; сложение
    call calc_sum
    print_sum_message
    printnum num_c    

    ; Обнуляем num_c перед вычислением произведения
    mov di, offset num_c 
    mov cx, max_len      
    zero_num_c:
    mov byte ptr [di], 0 
    inc di               
    loop zero_num_c

    scan_num num_a
    scan_num num_b
    xor dx, dx
    mov num_c[0], dh 

    ; умножение
    call calc_prod   
    print_prod_message
    printnum num_c
    
    endprogram
code ends
end start
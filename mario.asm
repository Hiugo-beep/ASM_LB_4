CSEG segment
    org 100h
    Begin: 
    call field_print
    call mario_print   
    call money_print 
    call map_print
    game_process:             
        mov ah, 11h
        int 16h 
        JE game_process_next
        CMP jump ,0
        JNE game_process_next
        mov bx , X_CONST
        CMP X,bx 
        JNE game_process_next
        mov ah, 10h
        int 16h 
   game_process_next: 
        CMP ah, 1h          ;ESC 
        JE end 
        CMP ah, 48h          ;^
        JNE next1
        CMP jump,0
        JE add_jump
        mov ah, 10h
        int 16h
        JMP next1 
    add_jump: 
        mov bx, X_CONST
        add jump,bx
    next1:
        CMP ah, 4Bh          ;<- 
        JNE next2  
        xor ax,ax 
        CMP Y,0
        JE map_shift_left
        SUB Y, 2 
        JMP next3
        map_shift_left:
        CMP count_map,0
        JE  next3
        sub count_map,2
        JMP next3
    next2:
        CMP ah, 4Dh           ;->
        JNE next3
        xor ax,ax 
        CMP Y, 50
        JE map_shift_right
        add Y,2
        JMP next3
    map_shift_right:
        add count_map,2 
    next3: 
        CMP jump ,0     
        JE not_jump    
        SUB X , 1
        SUB jump ,1
        JMP not_arrows
    not_jump:
        mov bx, X_CONST
        CMP X, bx
        JE not_arrows
        ADD X, 1 
    not_arrows:
        mov check_xx ,90
        mov bx , check_xx
        sub bx, count_map 
        CMP Y, bx
        JG x_const_change  
        mov X_const ,13
        JMP not_x_const_change 
    x_const_change:
        mov check_xx ,108
        mov bx , check_xx
        sub bx, count_map 
        CMP Y, bx
        JL x_c_change 
        mov X_const,13
        JMP not_x_const_change 
    x_c_change:
        mov X_const,8
    not_x_const_change:
        CMP X_const, 8
        JE check_x 
        JMP nextnext
    check_x:
        CMP X, 8
        JBE nextnext
        mov X, 8
    nextnext:
        call field_print
        call mario_print   
        call money_print
        call map_print
        call win_print
        call monstr_print 
        call block_print 
        call pause
        CMP game_over, 0
        JE game_process
    end:       
        call pause
        CMP win_flag,1
        JNE looser
        mov ah,9
	    mov dx,offset winning_line 
	    int 21h 
	    JMP not_looser
    looser:
        mov ah,9
	    mov dx,offset looser_line
	    int 21h
	not_looser: 
	    mov ah,9
	    mov dx,offset input 
	    int 21h
	    mov ah, 10h
    int 16h
;call field_clear          
    int 20h                       ;Exit from the program 
;//////////////////////////////////////////PAUSE///////////////////////////
pause PROC NEAR USES cx,dx,ax
    mov cx, 0h
    mov dx,61A8h
    mov ah, 86h
    int 15h  
    RET
pause ENDP   

;//////////////////////////////////////////FIELD PRINT/////////////////////         
field_print PROC NEAR USES ax,bx,cx,di,es 
    mov ax,0B800h               ;access to vide memory
    mov es,ax 
    mov di,0
    mov bx, offset level 
    mov ax, [bx]
    mov cx, level_size
    output:
       mov es:[di], ax
       add di,2         
       add bx,2
       mov ax, [bx]
    loop output       
    RET
field_print ENDP    
;//////////////////////////////////////////FIELD_CLEAR PRINT/////////////////////         
field_clear PROC NEAR USES ax,bx,cx,di,es 
    mov ax,0B800h               ;access to vide memory
    mov es,ax 
    mov di,0 
    mov ax, 0
    mov cx, level_size
    output1:
        mov es:[di], ax
       add di,2         
    loop output1       
    RET
field_clear ENDP 
;//////////////////////////////////////////MAP PRINT///////////////////// 
map_print PROC NEAR USES ax,bx,cx,dx,di,es
        mov ax,0B800h               ;access to vide memory
        mov es,ax   
        mov cx, MAP_SIZE
        mov bx, offset MAP
        mov ax, [bx]
    output_map:    
        mov ax, [bx]
        mov dx,ax
        xor ax,ax
        mov al,dh 
        mov di,160
        MUL di
        mov buf,ax
        mov di,ax 
        sub di,count_map 
        mov dx, [bx]
        mov dh,0   
        add di,dx  
    not_check:
        add buf, 9Ch
        CMP di,buf
        JG end_m
        add bx, 2  
        mov dx, [bx] 
        CMP dx, 0B020h
        JE end_m
        CMP es:[di] , 0B020h
        JNE is_money
        mov es:[di], dx
        JMP end_m
    is_money:
        mov [bx], 0B020h 
        inc money  
    end_m:   
        add bx, 2
    loop output_map
    RET     
map_print ENDP  
;//////////////////////////////////////////WIN PRINT///////////////////// 
win_print PROC NEAR USES ax,bx,cx,dx,di,es
        mov ax,0B800h               ;access to vide memory
        mov es,ax   
        mov cx, SIZE_WIN
        mov bx, offset win_line
        mov ax, [bx]
    output_win:    
        mov ax, [bx]
        mov dx,ax
        xor ax,ax
        mov al,dh 
        mov di,160
        MUL di
        mov buf,ax
        mov di,ax 
        sub di,count_map 
        mov dx, [bx]
        mov dh,0   
        add di,dx  
        add buf, 9Ch 
        CMP di,buf
        JG endd
        add bx, 2  
        mov dx, [bx] 
        CMP dx, 0B020h
        JE end_m1
        CMP es:[di] , 0B020h
        JNE is_end
        mov es:[di], dx
        JMP end_m1
        is_end:
        mov win_flag,1
        inc game_over   
        end_m1:   
        add bx, 2
    loop output_win
    RET 
    endd:    
win_print ENDP
;//////////////////////////////////////////MARIO PRINT///////////////////// 
mario_print PROC NEAR USES ax,bx,cx,di,es
        mov ax,0B800h               ;access to vide memory
        mov es,ax 
    try_again:
        mov di,160
         mov ax,X
        MUL di
        mov di,ax
        add di, Y
        mov bx, offset mario 
        mov ax, [bx]
        mov cx, MARIO_SIZE
        mov count,0
     output_mario:
        CMP es:[di] , 0A020h
        JNE continueee
        SUB Y,2
        JMP try_again
     continueee:
        mov es:[di], ax
        add di,2         
        add bx,2
        mov ax, [bx] 
        inc count 
        CMP count,5
        JNE not_new_line
        add di,160
        sub di,10 
        mov count,0
      not_new_line:
    loop output_mario
    RET
mario_print ENDP   
;//////////////////////////////////////////BLOCK PRINT///////////////////// 
block_print PROC NEAR USES ax,bx,cx,di,es
        mov ax,0B800h               ;access to vide memory
        mov es,ax 
        mov di,160
        mov ax,X2
        MUL di
        mov di,ax
        add di, Y2
        mov bx, offset block 
        mov ax, [bx]
        mov cx, SIZE_BLOCK
        sub di, count_map
        mov count,0
   output_block:
        mov es:[di], ax
        add di,2         
        add bx,2
        mov ax, [bx] 
        inc count 
        CMP count,5
        JNE not_new_line3
        add di,160
        sub di,10 
        mov count,0
    not_new_line3:
    loop output_mario
    RET
block_print ENDP
;//////////////////////////////////////////MONSTR PRINT///////////////////// 
monstr_print PROC NEAR USES ax,bx,cx,di,es 
    CMP count_m1,0
    JE nonext
    JMP no_nonext
    nonext:
    mov flag_m, 0  
    no_nonext: 
    CMP count_m1,10 
    JE npnext  
    JMP not_npnext 
    npnext:
    mov flag_m, 1 
    not_npnext:
    CMP  flag_m, 0
    JE null 
    JMP not_null
    null: 
    inc count_m1 
    add Y1, 2
    JMP nullnull 
     not_null:
     dec count_m1
     sub Y1 ,2 
     nullnull:
    mov ax,0B800h               ;access to vide memory
    mov es,ax 
    mov di,160
    mov ax,X1
    MUL di
    mov di,ax
    add di, Y1
    mov bx, offset knoll 
    mov dx, [bx]
    mov cx, SIZE_KNOLL
   sub di,count_map
    mov count,0
     output_monstr: 
       CMP es:[di] , 0B020h
       JNE iss_end
       mov es:[di], ax
       add di,2         
       add bx,2
       mov dx, [bx] 
       inc count 
       CMP count,2
       JNE not_new_line1
       add di,160
       sub di,4 
       mov count,0
       not_new_line1:
    loop output_monstr
     JMP continue
    iss_end:
    inc game_over
    continue:
    RET
monstr_print ENDP
;//////////////////////////////////////////MONEY PRINTT///////////////////// 
money_print PROC NEAR USES ax,bx,cx,di,es  
        mov ax,0B800h               ;access to vide memory
        mov es,ax 
        mov di, money_offset
        mov bx, offset money_str 
        mov cx, 2
    output_money_str:
        mov ax, [bx]
        mov es:[di], ax
        add bx, 2
        add di, 2
    loop output_money_str
        xor ax,ax 
        mov al, money
        mov money_out, al
        CMP money_out,0
        JNE output_money_preparation
        mov ax, 0B030h       ;0
        mov es:[di], ax
    RET
    output_money_preparation:
        mov cx, 3
        mov bx, offset money_buf 
        mov ax, '$'
        mov [bx],ax
        mov [bx+2],ax
        mov [bx+4],ax
        add bx, 4 
    output_money:
        CMP money_out,0
        JNE not_zero
        JMP output_buf_preparation
        not_zero: 
            xor ax,ax
            mov al, money_out
            mov del,10
            IDIV del
            mov money_out,al 
            mov al, ah
            add al, 30h
            mov ah, 30h 
            mov [bx], ax  
            sub bx,2         
    loop output_money 
    output_buf_preparation:
    mov cx, 3
    mov bx, offset money_buf
    output_buf: 
        mov ax, [bx]
        CMP ax,'$'
        JE zero
        mov es:[di], ax
        add di, 2
        zero:
        add bx,2    
    loop output_buf
    RET
money_print ENDP    
;//////////////////////////////////////////DATA SEGMENT/////////////////////                       
X dw 13 
X_CONST dw 13
Y dw 0 
MARIO_ROW EQU 7
MARIO_COLUMN EQU 5 
count dw 0
MARIO_SIZE EQU MARIO_ROW*MARIO_COLUMN         ;
mario DW 0B020h,3CDBh,3CDBh,3CDBh,0B020h,    ;0B020h,0C020h,0C020h,0C020h,0B020h,  ;
      DW 0B020h,3CDBh,3CDBh,3CDBh,3CDBh,    ;0B020h,0C020h,0C020h,0C020h,0C020h,  ;
      DW  0B020h,3EDBh,30DBh,3EDBh,0B020h,           ;0B020h,0F020h,20h,0F020h, 0B020h,    ;DW 0B020h,0E020h,20h,0E020h, 0B020h,
      DW  0B020h,3EDBh,3EDBh,36DBh,0B020h,           ;0B020h,0F020h,0F020h,6020h,0B020h,   ; DW 0B020h,0E020h,0E020h,6020h,0B020h,
      DW  3EDBh,3CDBh,3CDBh,3CDBh,3EDBh,          ;0F020h,0C020h,0C020h,0C020h, 0F020h,  ; DW 0E020h,0C020h,0C020h,0C020h, 0E020h,
      DW  0B020h,3CDBh,3CDBh,3CDBh, 0B020h,          ;0B020h,0C020h,0C020h,0C020h,0B020h,   ;
      DW   0B020h,36DBh,0B020h,36DBh,0B020h,      ;0B020h,6020h,0B020h,6020h,0B020h     ;
ROWS EQU 25         
COLUMNS EQU 80 
MAP_SIZE dw 18

MAP DW 1230h ,3EDBh,  
    DW 0930h ,3EDBh,
    DW 1242h ,3EDBh,  
    DW 0942h ,3EDBh,  
    DW 1248h ,3EDBh,  
    DW 0948h ,3EDBh,
    DW 1252h ,3EDBh,  
    DW 0952h ,3EDBh,  
    DW 127Ah ,3EDBh,  
    DW 97Ah ,3EDBh,
    DW 1284h ,3EDBh,  
    DW 984h ,3EDBh,  
    DW 1298h ,3EDBh,  
    DW 998h ,3EDBh,
    DW 12A2h ,3EDBh,  
    DW 9A2h ,3EDBh,  
    DW 127Ah ,3EDBh,  
    DW  97Ah ,3EDBh,  
SIZE_WIN dw 15
win_line  DW   03E0h , 0A020h,  
          DW 04E0h , 0A020h,  
          DW 05E0h , 0A020h,  
          DW 06E0h , 0A020h,  
          DW 07E0h , 0A020h,  
          DW 08E0h , 0A020h,  
          DW 09E0h , 0A020h,  
          DW 0AE0h , 0A020h,  
          DW 0BE0h , 0A020h, 
          DW 0CE0h , 0A020h,  
          DW 0DE0h , 0A020h,  
          DW 0EE0h , 0A020h,  
          DW 0FE0h , 0A020h,  
          DW 10E0h , 0A020h,  
          DW 11E0h , 0A020h,  
X2 dw 15 
Y2 dw 100
SIZE_BLOCK dw 25
block DW 0A020h,0A020h,0A020h,0A020h,0A020h,
      DW 0A020h, 0A020h , 0A020h, 0A020h, 0A020h,
      DW 0A020h,0A020h,0A020h,0A020h,0A020h,
      DW 0A020h,0A020h,0A020h,0A020h,0A020h,
      DW 0A020h,0A020h,0A020h,0A020h,0A020h,    
SIZE_KNOLL dw 4
knoll DW 3CDBh , 3CDBh, 3CDBh, 3CDBh  
X1 dw 18
Y1 dw 40
count_m1 dw 10 
flag_m dw 0
winning_line db 'Congratulations, You Won!!!',13,10,'$'
looser_line db   'Unfortunately, You Lost,please, try again...',13,10,'$'     
input db 'press the key...',13,10,'$'
buf dw 0    
count_map dw ,0
level DW 20*COLUMNS DUP(0B020h),
      DW 5*COLUMNS DUP(0A020h)   
level_size EQU ROWS*COLUMNS  
money_offset EQU 39*2
money_str DW 3E02h,3078h 
money db 0
jump dw 0
money_out db 0
money_buf dw 3 DUP ('$')
del db 1
check_xx dw 0
win_flag db 0
symbol dw 0
game_over dw 0  
CSEG ends     
end Begin

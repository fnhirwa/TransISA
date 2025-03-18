.section .data
msg db 'Hello World!', 0Ah
SECTION .text
 global  _start
_start:
 mov edx, 13
lea ecx, [msg]
mov ebx, 1
mov eax, 4
int 80h
end
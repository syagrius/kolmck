  asm
    pxor mm0, mm0
    pxor mm1, mm1
    xor  eax, eax
    inc  eax
    movd mm1, eax
    movd mm2, eax
    psllq mm2, 32
    mov  edx, [UnpackedSize]
    movd mm3, edx
    psllq mm3, 32
    mov  edx, [PackedSize]
    pxor  mm4, mm4
    movd  mm4, edx
    por  mm3, mm4 // mm3 hi dword = UnpackedSize, mm3 lo dword = PackedSize
    pxor mm4, mm4 // mm4 = Data
    mov  eax, [CodeMask]
    movd mm5, eax
    mov  eax, [OldCode]
    movd mm6, eax
    //

    push ebx
    push esi
    push edi

    mov esi, [SourcePtr]
    mov edi, [Target]
    {$IFDEF GIF_SAFE}
    xor ebx, ebx
    {$ELSE}
    mov ebx, [StackPointer]
    {$ENDIF}
    mov ch, byte ptr [Bits]
    mov cl, byte ptr [CodeSize]

  @@loop:
    {$IFDEF GIF_SAFE}
    cmp   [Bad], 0
    jnz   @@exit
    {$ENDIF}

    movq    mm7, mm3 // check if (PackedSize > 0) and (UnpackedSize > 0) then
    pcmpgtd mm7, mm0
    packsswb mm7, mm7
    movd    eax, mm7
    inc     eax
    jnz     @@exit   // end of loop if not (one of operands = 0)

    {$IFDEF GIF_LOG}
    pushad
    movq mm7, mm3
    movd edx, mm7
    psrlq mm7, 32
    movd eax, mm7
    movd ecx, mm4
    call doGifLog
    popad
    {$ENDIF}

    movzx eax, byte ptr [esi]
    xchg  cl, ch
    shl   eax, cl
    xchg  cl, ch
    //add   [Data], eax
    movd  mm7, eax
    paddd mm4, mm7
    add   ch, 8

  @@while:
    {$IFDEF GIF_SAFE}
    cmp   [Bad], 0
    jnz   @@exit
    {$ENDIF}

    cmp   ch, cl
    jb    @@e_while

    //mov   eax, [Data]
    movd  eax, mm4
    //and   eax, [CodeMask]
    movd  edx, mm5
    and   eax, edx
    //shr   [Data], cl
    movzx edx, cl
    movd  mm7, edx
    psrld mm4, mm7

    sub   ch, cl
    cmp   eax, [EOICode]
    jz    @@e_while

  @@1:
    cmp   eax, [ClearCode]
    jnz   @@2

    mov   cl, [initial_code_size]
    inc   cl
    xor   eax, eax
    inc   eax
    shl   eax, cl
    dec   eax
    //mov   [CodeMask], eax
    movd  mm5, eax
    mov   eax, [ClearCode]
    add   eax, 2
    mov   [FreeCode], eax
    //mov   [OldCode], NoLZWCode
    mov   edx, NoLZWCode
    movd  mm6, edx
    jmp   @@while

  @@2:
    cmp   eax, [FreeCode]
    ja    @@e_while

  @@3:
    //cmp   [OldCode], NoLZWCode
    movd  edx, mm6
    cmp   edx, NoLZWCode
    jne   @@4

    //mov   [OldCode], eax
    movd  mm6, eax
    {$IFDEF GIF_SAFE}
    cmp   eax, GIFBufSize-1
    jle   @@suffix_good1
    mov   [Bad], 1
    and   eax, GIFBufSize-1
  @@suffix_good1:
    {$ENDIF}
    mov   al, byte ptr [suffix+eax]
    mov   [FirstChar], al
    {$IFDEF GIF_LOG}
    pushad
    call  doGifLog2
    popad
    {$ENDIF}
    stosb
    //dec   [UnpackedSize]
    psubd mm3, mm2
    jmp   @@while

  @@4:
    //mov   [InCode], eax
    movd  mm7, eax
    cmp   eax, [FreeCode]
    jne   @@5

    mov   al, [FirstChar]
    {$IFDEF GIF_SAFE}
    cmp   ebx, GIFBufSize
    jae   @@stk_bad1
    mov   byte ptr [Stack+ebx], al
    inc   ebx
    jmp   @@stk_good1
  @@stk_bad1:
    mov   [Bad], 1
  @@stk_good1:
    {$ELSE}
    mov   [ebx], al
    inc   ebx
    {$ENDIF}
    //mov   eax, [OldCode]
    movd  eax, mm6

  @@5:
    cmp   eax, [ClearCode]
    jbe   @@6

    {$IFDEF GIF_SAFE}
    cmp   eax, GIFBufSize-1
    jle   @@suffix_good2
    and   eax, GIFBufSize-1
    mov   [Bad], 1
  @@suffix_good2:
    {$ENDIF}
    mov   dl, byte ptr [Suffix+eax]
    {$IFDEF GIF_SAFE}
    cmp   ebx, GIFBufSize
    jb    @@stk_good2
    mov   [Bad], 1
    jmp   @@exit
  @@stk_good2:
    mov   byte ptr [Stack+ebx], dl
    inc   ebx
  @@stk_bad2:
    cmp   eax, GIFBufSize-1
    jle   @@Prefix_good1
    and   eax, GIFBufSize-1
    mov   [Bad], 1
  @@Prefix_good1:
    {$ELSE}
    mov   byte ptr [ebx], dl
    inc   ebx
    {$ENDIF}
    mov   eax, dword ptr [Prefix+eax*4]
    jmp   @@5

  @@6:
    {$IFDEF GIF_SAFE}
    cmp   eax, GIFBufSize-1
    jle   @@suffix_good3
    and   eax, GIFBufSize-1
    mov   [Bad], 1
  @@suffix_good3:
    {$ENDIF}
    mov   dl, byte ptr [Suffix+eax]
    mov   [FirstChar], dl
    {$IFDEF GIF_SAFE}
    cmp   ebx, GIFBufSize
    jae   @@stk_bad3
    mov   byte ptr [Stack+ebx], dl
    inc   ebx
    jmp   @@stk_good3
  @@stk_bad3:
    mov   [Bad], 1
  @@stk_good3:
    {$ELSE}
    mov   byte ptr [ebx], dl
    inc   ebx
    {$ENDIF}

    mov   eax, [FreeCode]
    {$IFDEF GIF_SAFE}
    cmp   eax, GIFBufSize-1
    jle   @@suffix_good4
    and   eax, GIFBufSize-1
    mov   [Bad], 1
  @@suffix_good4:
    {$ENDIF}
    mov   byte ptr [Suffix+eax], dl
    //mov   edx, [OldCode]
    movd  edx, mm6
    {$IFDEF GIF_SAFE}
    cmp   edx, GIFBufSize-1
    jle   @@Prefix_good2
    and   edx, GIFBufSize-1
    mov   [Bad], 1
  @@Prefix_good2:
    {$ENDIF}
    mov   dword ptr [Prefix+eax*4], edx

    //cmp   eax, [CodeMask]
    movd  edx, mm5
    cmp   eax, edx
    jnz   @@7
    cmp   cl, 12
    jae   @@7

    inc   cl
    xor   eax, eax
    inc   eax
    shl   eax, cl
    dec   eax
    //mov   [CodeMask], eax
    movd  mm5, eax

  @@7:
    cmp   [FreeCode], GIFBufSize-1
    jae   @@8
    inc   [FreeCode]

  @@8:
    //mov   eax, [InCode]
    movd  eax, mm7
    //mov   [OldCode], eax
    movd  mm6, eax

  @@9:
    {$IFDEF GIF_SAFE}
    test  ebx, ebx
    jz    @@stk_bad4
    dec   ebx
    mov   al, byte ptr [Stack+ebx]
    jmp   @@stk_good4
  @@stk_bad4:
    mov   [Bad], 1
  @@stk_good4:
    {$ELSE}
    dec   ebx
    mov   al, byte ptr [ebx]
    {$ENDIF}

    {$IFDEF GIF_LOG}
    pushad
    call  doGifLog2
    popad
    {$ENDIF}
    stosb
    //dec   [UnpackedSize]
    psubd mm3, mm2
    {$IFDEF GIF_SAFE}
    test  ebx, ebx
    {$ELSE}
    cmp   ebx, [StackPointer]
    {$ENDIF}

    jne   @@9

    jmp   @@while

  @@e_while:
    inc   esi
    //dec   [PackedSize]
    psubd mm3, mm1
    jmp   @@loop

  @@exit:
    pop  edi
    pop  esi
    pop  ebx

    emms
  end

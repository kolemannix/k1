	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 26, 0	sdk_version 26, 1
	.file	0 "/Users/knix/dev/k1" "test_src/modules_test/libs/foo.c" md5 0xf99fb812e329f4fce9e19598c25df56f
	.globl	_very_small                     ; -- Begin function very_small
	.p2align	2
_very_small:                            ; @very_small
Lfunc_begin0:
	.loc	0 6 0                           ; test_src/modules_test/libs/foo.c:6:0
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #48
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	add	x29, sp, #32
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	x8, x0
	sturb	w8, [x29, #-2]
	mov	x8, x1
	sturb	w8, [x29, #-3]
Ltmp0:
	.loc	0 7 13 prologue_end             ; test_src/modules_test/libs/foo.c:7:13
	sturb	wzr, [x29, #-1]
	.loc	0 8 12                          ; test_src/modules_test/libs/foo.c:8:12
	ldursb	w9, [x29, #-2]
	.loc	0 8 7 is_stmt 0                 ; test_src/modules_test/libs/foo.c:8:7
	ldursb	w8, [x29, #-1]
	add	w8, w8, w9
	sturb	w8, [x29, #-1]
	.loc	0 9 12 is_stmt 1                ; test_src/modules_test/libs/foo.c:9:12
	ldursb	w9, [x29, #-3]
	.loc	0 9 7 is_stmt 0                 ; test_src/modules_test/libs/foo.c:9:7
	ldursb	w8, [x29, #-1]
	add	w8, w8, w9
	sturb	w8, [x29, #-1]
	.loc	0 10 52 is_stmt 1               ; test_src/modules_test/libs/foo.c:10:52
	ldursb	w12, [x29, #-2]
	.loc	0 10 57 is_stmt 0               ; test_src/modules_test/libs/foo.c:10:57
	ldursb	w11, [x29, #-3]
	.loc	0 10 62                         ; test_src/modules_test/libs/foo.c:10:62
	ldursb	w10, [x29, #-1]
	.loc	0 10 3                          ; test_src/modules_test/libs/foo.c:10:3
	mov	x9, sp
                                        ; implicit-def: $x8
	mov	x8, x12
	str	x8, [x9]
                                        ; implicit-def: $x8
	mov	x8, x11
	str	x8, [x9, #8]
                                        ; implicit-def: $x8
	mov	x8, x10
	str	x8, [x9, #16]
	adrp	x0, l_.str@PAGE
	add	x0, x0, l_.str@PAGEOFF
	bl	_printf
	.loc	0 11 3 is_stmt 1                ; test_src/modules_test/libs/foo.c:11:3
	ldurb	w0, [x29, #-1]
	.loc	0 11 3 epilogue_begin is_stmt 0 ; test_src/modules_test/libs/foo.c:11:3
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #48
	ret
Ltmp1:
Lfunc_end0:
	.cfi_endproc
                                        ; -- End function
	.globl	_small                          ; -- Begin function small
	.p2align	2
_small:                                 ; @small
Lfunc_begin1:
	.loc	0 15 0 is_stmt 1                ; test_src/modules_test/libs/foo.c:15:0
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	mov	x8, x0
	str	w8, [sp, #8]
	mov	x8, x1
	str	w8, [sp, #4]
Ltmp3:
	.loc	0 16 9 prologue_end             ; test_src/modules_test/libs/foo.c:16:9
	str	wzr, [sp, #12]
	.loc	0 17 12                         ; test_src/modules_test/libs/foo.c:17:12
	ldrsb	w9, [sp, #8]
	.loc	0 17 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:17:7
	ldrsb	w8, [sp, #12]
	add	w8, w8, w9
	strb	w8, [sp, #12]
	.loc	0 18 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:18:12
	ldrsb	w9, [sp, #9]
	.loc	0 18 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:18:7
	ldrsb	w8, [sp, #13]
	add	w8, w8, w9
	strb	w8, [sp, #13]
	.loc	0 19 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:19:12
	ldrsb	w9, [sp, #10]
	.loc	0 19 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:19:7
	ldrsb	w8, [sp, #14]
	add	w8, w8, w9
	strb	w8, [sp, #14]
	.loc	0 20 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:20:12
	ldrsb	w9, [sp, #11]
	.loc	0 20 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:20:7
	ldrsb	w8, [sp, #15]
	add	w8, w8, w9
	strb	w8, [sp, #15]
	.loc	0 21 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:21:12
	ldrsb	w9, [sp, #4]
	.loc	0 21 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:21:7
	ldrsb	w8, [sp, #12]
	add	w8, w8, w9
	strb	w8, [sp, #12]
	.loc	0 22 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:22:12
	ldrsb	w9, [sp, #5]
	.loc	0 22 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:22:7
	ldrsb	w8, [sp, #13]
	add	w8, w8, w9
	strb	w8, [sp, #13]
	.loc	0 23 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:23:12
	ldrsb	w9, [sp, #6]
	.loc	0 23 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:23:7
	ldrsb	w8, [sp, #14]
	add	w8, w8, w9
	strb	w8, [sp, #14]
	.loc	0 24 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:24:12
	ldrsb	w9, [sp, #7]
	.loc	0 24 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:24:7
	ldrsb	w8, [sp, #15]
	add	w8, w8, w9
	strb	w8, [sp, #15]
	.loc	0 25 3 is_stmt 1                ; test_src/modules_test/libs/foo.c:25:3
	ldr	w0, [sp, #12]
	.loc	0 25 3 epilogue_begin is_stmt 0 ; test_src/modules_test/libs/foo.c:25:3
	add	sp, sp, #16
	ret
Ltmp4:
Lfunc_end1:
	.cfi_endproc
                                        ; -- End function
	.globl	_medium                         ; -- Begin function medium
	.p2align	2
_medium:                                ; @medium
Lfunc_begin2:
	.loc	0 29 0 is_stmt 1                ; test_src/modules_test/libs/foo.c:29:0
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #48
	.cfi_def_cfa_offset 48
	str	x0, [sp, #16]
	str	x1, [sp, #24]
	str	x2, [sp]
	str	x3, [sp, #8]
Ltmp6:
	.loc	0 30 10 prologue_end            ; test_src/modules_test/libs/foo.c:30:10
	str	xzr, [sp, #32]
	str	xzr, [sp, #40]
	.loc	0 31 12                         ; test_src/modules_test/libs/foo.c:31:12
	ldr	x9, [sp, #16]
	.loc	0 31 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:31:7
	ldr	x8, [sp, #32]
	add	x8, x8, x9
	str	x8, [sp, #32]
	.loc	0 32 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:32:12
	ldr	x9, [sp, #24]
	.loc	0 32 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:32:7
	ldr	x8, [sp, #40]
	add	x8, x8, x9
	str	x8, [sp, #40]
	.loc	0 33 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:33:12
	ldr	x9, [sp]
	.loc	0 33 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:33:7
	ldr	x8, [sp, #32]
	add	x8, x8, x9
	str	x8, [sp, #32]
	.loc	0 34 12 is_stmt 1               ; test_src/modules_test/libs/foo.c:34:12
	ldr	x9, [sp, #8]
	.loc	0 34 7 is_stmt 0                ; test_src/modules_test/libs/foo.c:34:7
	ldr	x8, [sp, #40]
	add	x8, x8, x9
	str	x8, [sp, #40]
	.loc	0 35 3 is_stmt 1                ; test_src/modules_test/libs/foo.c:35:3
	ldr	x0, [sp, #32]
	ldr	x1, [sp, #40]
	.loc	0 35 3 epilogue_begin is_stmt 0 ; test_src/modules_test/libs/foo.c:35:3
	add	sp, sp, #48
	ret
Ltmp7:
Lfunc_end2:
	.cfi_endproc
                                        ; -- End function
	.globl	_large                          ; -- Begin function large
	.p2align	2
_large:                                 ; @large
Lfunc_begin3:
	.loc	0 41 0 is_stmt 1                ; test_src/modules_test/libs/foo.c:41:0
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	mov	x9, x8
	mov	x8, x0
	str	x8, [sp, #8]
	mov	x8, x1
	str	x8, [sp]
Ltmp9:
	;DEBUG_VALUE: large:r <- [$x9+0]
	.loc	0 42 9 prologue_end             ; test_src/modules_test/libs/foo.c:42:9
	movi.2d	v0, #0000000000000000
	str	q0, [x9]
	str	q0, [x9, #16]
	.loc	0 43 22                         ; test_src/modules_test/libs/foo.c:43:22
	ldr	x10, [x0]
	.loc	0 43 12 is_stmt 0               ; test_src/modules_test/libs/foo.c:43:12
	ldr	x8, [x9]
	add	x8, x8, x10
	str	x8, [x9]
	.loc	0 44 22 is_stmt 1               ; test_src/modules_test/libs/foo.c:44:22
	ldr	x10, [x0, #8]
	.loc	0 44 12 is_stmt 0               ; test_src/modules_test/libs/foo.c:44:12
	ldr	x8, [x9, #8]
	add	x8, x8, x10
	str	x8, [x9, #8]
	.loc	0 45 22 is_stmt 1               ; test_src/modules_test/libs/foo.c:45:22
	ldr	x10, [x0, #16]
	.loc	0 45 12 is_stmt 0               ; test_src/modules_test/libs/foo.c:45:12
	ldr	x8, [x9, #16]
	add	x8, x8, x10
	str	x8, [x9, #16]
	.loc	0 46 22 is_stmt 1               ; test_src/modules_test/libs/foo.c:46:22
	ldr	x10, [x0, #24]
	.loc	0 46 12 is_stmt 0               ; test_src/modules_test/libs/foo.c:46:12
	ldr	x8, [x9, #24]
	add	x8, x8, x10
	str	x8, [x9, #24]
	.loc	0 47 22 is_stmt 1               ; test_src/modules_test/libs/foo.c:47:22
	ldr	x10, [x1]
	.loc	0 47 12 is_stmt 0               ; test_src/modules_test/libs/foo.c:47:12
	ldr	x8, [x9]
	add	x8, x8, x10
	str	x8, [x9]
	.loc	0 48 22 is_stmt 1               ; test_src/modules_test/libs/foo.c:48:22
	ldr	x10, [x1, #8]
	.loc	0 48 12 is_stmt 0               ; test_src/modules_test/libs/foo.c:48:12
	ldr	x8, [x9, #8]
	add	x8, x8, x10
	str	x8, [x9, #8]
	.loc	0 49 22 is_stmt 1               ; test_src/modules_test/libs/foo.c:49:22
	ldr	x10, [x1, #16]
	.loc	0 49 12 is_stmt 0               ; test_src/modules_test/libs/foo.c:49:12
	ldr	x8, [x9, #16]
	add	x8, x8, x10
	str	x8, [x9, #16]
	.loc	0 50 22 is_stmt 1               ; test_src/modules_test/libs/foo.c:50:22
	ldr	x10, [x1, #24]
	.loc	0 50 12 is_stmt 0               ; test_src/modules_test/libs/foo.c:50:12
	ldr	x8, [x9, #24]
	add	x8, x8, x10
	str	x8, [x9, #24]
	.loc	0 51 3 epilogue_begin is_stmt 1 ; test_src/modules_test/libs/foo.c:51:3
	add	sp, sp, #16
	ret
Ltmp10:
Lfunc_end3:
	.cfi_endproc
                                        ; -- End function
	.globl	_hfa_small                      ; -- Begin function hfa_small
	.p2align	2
_hfa_small:                             ; @hfa_small
Lfunc_begin4:
	.loc	0 55 0                          ; test_src/modules_test/libs/foo.c:55:0
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	s0, [sp, #8]
	str	s1, [sp, #12]
Ltmp12:
	.loc	0 56 3 prologue_end epilogue_begin ; test_src/modules_test/libs/foo.c:56:3
	add	sp, sp, #16
	ret
Ltmp13:
Lfunc_end4:
	.cfi_endproc
                                        ; -- End function
	.globl	_hfa_big                        ; -- Begin function hfa_big
	.p2align	2
_hfa_big:                               ; @hfa_big
Lfunc_begin5:
	.loc	0 59 0                          ; test_src/modules_test/libs/foo.c:59:0
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	.cfi_def_cfa_offset 32
	str	d0, [sp]
	str	d1, [sp, #8]
	str	d2, [sp, #16]
	str	d3, [sp, #24]
Ltmp15:
	.loc	0 60 3 prologue_end epilogue_begin ; test_src/modules_test/libs/foo.c:60:3
	add	sp, sp, #32
	ret
Ltmp16:
Lfunc_end5:
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_.str:                                 ; @.str
	.asciz	"very small, a.x=%d, b.x=%d, r.x=%d\n"

	.file	1 "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types" "_uint64_t.h" md5 0x77fc5e91653260959605f129691cf9b1
	.section	__DWARF,__debug_abbrev,regular,debug
Lsection_abbrev:
	.byte	1                               ; Abbreviation Code
	.byte	17                              ; DW_TAG_compile_unit
	.byte	1                               ; DW_CHILDREN_yes
	.byte	37                              ; DW_AT_producer
	.byte	37                              ; DW_FORM_strx1
	.byte	19                              ; DW_AT_language
	.byte	5                               ; DW_FORM_data2
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.ascii	"\202|"                         ; DW_AT_LLVM_sysroot
	.byte	37                              ; DW_FORM_strx1
	.ascii	"\357\177"                      ; DW_AT_APPLE_sdk
	.byte	37                              ; DW_FORM_strx1
	.byte	114                             ; DW_AT_str_offsets_base
	.byte	23                              ; DW_FORM_sec_offset
	.byte	16                              ; DW_AT_stmt_list
	.byte	23                              ; DW_FORM_sec_offset
	.byte	27                              ; DW_AT_comp_dir
	.byte	37                              ; DW_FORM_strx1
	.byte	17                              ; DW_AT_low_pc
	.byte	27                              ; DW_FORM_addrx
	.byte	18                              ; DW_AT_high_pc
	.byte	6                               ; DW_FORM_data4
	.byte	115                             ; DW_AT_addr_base
	.byte	23                              ; DW_FORM_sec_offset
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	2                               ; Abbreviation Code
	.byte	52                              ; DW_TAG_variable
	.byte	0                               ; DW_CHILDREN_no
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	2                               ; DW_AT_location
	.byte	24                              ; DW_FORM_exprloc
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	3                               ; Abbreviation Code
	.byte	1                               ; DW_TAG_array_type
	.byte	1                               ; DW_CHILDREN_yes
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	4                               ; Abbreviation Code
	.byte	33                              ; DW_TAG_subrange_type
	.byte	0                               ; DW_CHILDREN_no
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	55                              ; DW_AT_count
	.byte	11                              ; DW_FORM_data1
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	5                               ; Abbreviation Code
	.byte	36                              ; DW_TAG_base_type
	.byte	0                               ; DW_CHILDREN_no
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	62                              ; DW_AT_encoding
	.byte	11                              ; DW_FORM_data1
	.byte	11                              ; DW_AT_byte_size
	.byte	11                              ; DW_FORM_data1
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	6                               ; Abbreviation Code
	.byte	36                              ; DW_TAG_base_type
	.byte	0                               ; DW_CHILDREN_no
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	11                              ; DW_AT_byte_size
	.byte	11                              ; DW_FORM_data1
	.byte	62                              ; DW_AT_encoding
	.byte	11                              ; DW_FORM_data1
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	7                               ; Abbreviation Code
	.byte	46                              ; DW_TAG_subprogram
	.byte	1                               ; DW_CHILDREN_yes
	.byte	17                              ; DW_AT_low_pc
	.byte	27                              ; DW_FORM_addrx
	.byte	18                              ; DW_AT_high_pc
	.byte	6                               ; DW_FORM_data4
	.byte	64                              ; DW_AT_frame_base
	.byte	24                              ; DW_FORM_exprloc
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	39                              ; DW_AT_prototyped
	.byte	25                              ; DW_FORM_flag_present
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	63                              ; DW_AT_external
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	8                               ; Abbreviation Code
	.byte	5                               ; DW_TAG_formal_parameter
	.byte	0                               ; DW_CHILDREN_no
	.byte	2                               ; DW_AT_location
	.byte	24                              ; DW_FORM_exprloc
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	9                               ; Abbreviation Code
	.byte	52                              ; DW_TAG_variable
	.byte	0                               ; DW_CHILDREN_no
	.byte	2                               ; DW_AT_location
	.byte	24                              ; DW_FORM_exprloc
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	10                              ; Abbreviation Code
	.byte	46                              ; DW_TAG_subprogram
	.byte	1                               ; DW_CHILDREN_yes
	.byte	17                              ; DW_AT_low_pc
	.byte	27                              ; DW_FORM_addrx
	.byte	18                              ; DW_AT_high_pc
	.byte	6                               ; DW_FORM_data4
	.ascii	"\347\177"                      ; DW_AT_APPLE_omit_frame_ptr
	.byte	25                              ; DW_FORM_flag_present
	.byte	64                              ; DW_AT_frame_base
	.byte	24                              ; DW_FORM_exprloc
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	39                              ; DW_AT_prototyped
	.byte	25                              ; DW_FORM_flag_present
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	63                              ; DW_AT_external
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	11                              ; Abbreviation Code
	.byte	46                              ; DW_TAG_subprogram
	.byte	1                               ; DW_CHILDREN_yes
	.byte	17                              ; DW_AT_low_pc
	.byte	27                              ; DW_FORM_addrx
	.byte	18                              ; DW_AT_high_pc
	.byte	6                               ; DW_FORM_data4
	.ascii	"\347\177"                      ; DW_AT_APPLE_omit_frame_ptr
	.byte	25                              ; DW_FORM_flag_present
	.byte	64                              ; DW_AT_frame_base
	.byte	24                              ; DW_FORM_exprloc
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	39                              ; DW_AT_prototyped
	.byte	25                              ; DW_FORM_flag_present
	.byte	63                              ; DW_AT_external
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	12                              ; Abbreviation Code
	.byte	22                              ; DW_TAG_typedef
	.byte	0                               ; DW_CHILDREN_no
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	13                              ; Abbreviation Code
	.byte	19                              ; DW_TAG_structure_type
	.byte	1                               ; DW_CHILDREN_yes
	.byte	11                              ; DW_AT_byte_size
	.byte	11                              ; DW_FORM_data1
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	14                              ; Abbreviation Code
	.byte	13                              ; DW_TAG_member
	.byte	0                               ; DW_CHILDREN_no
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	73                              ; DW_AT_type
	.byte	19                              ; DW_FORM_ref4
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	56                              ; DW_AT_data_member_location
	.byte	11                              ; DW_FORM_data1
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	0                               ; EOM(3)
	.section	__DWARF,__debug_info,regular,debug
Lsection_info:
Lcu_begin0:
.set Lset0, Ldebug_info_end0-Ldebug_info_start0 ; Length of Unit
	.long	Lset0
Ldebug_info_start0:
	.short	5                               ; DWARF version number
	.byte	1                               ; DWARF Unit Type
	.byte	8                               ; Address Size (in bytes)
.set Lset1, Lsection_abbrev-Lsection_abbrev ; Offset Into Abbrev. Section
	.long	Lset1
	.byte	1                               ; Abbrev [1] 0xc:0x215 DW_TAG_compile_unit
	.byte	0                               ; DW_AT_producer
	.short	29                              ; DW_AT_language
	.byte	1                               ; DW_AT_name
	.byte	2                               ; DW_AT_LLVM_sysroot
	.byte	3                               ; DW_AT_APPLE_sdk
.set Lset2, Lstr_offsets_base0-Lsection_str_off ; DW_AT_str_offsets_base
	.long	Lset2
.set Lset3, Lline_table_start0-Lsection_line ; DW_AT_stmt_list
	.long	Lset3
	.byte	4                               ; DW_AT_comp_dir
	.byte	1                               ; DW_AT_low_pc
.set Lset4, Lfunc_end5-Lfunc_begin0     ; DW_AT_high_pc
	.long	Lset4
.set Lset5, Laddr_table_base0-Lsection_info0 ; DW_AT_addr_base
	.long	Lset5
	.byte	2                               ; Abbrev [2] 0x25:0xa DW_TAG_variable
	.long	47                              ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	10                              ; DW_AT_decl_line
	.byte	2                               ; DW_AT_location
	.byte	161
	.byte	0
	.byte	3                               ; Abbrev [3] 0x2f:0xc DW_TAG_array_type
	.long	59                              ; DW_AT_type
	.byte	4                               ; Abbrev [4] 0x34:0x6 DW_TAG_subrange_type
	.long	63                              ; DW_AT_type
	.byte	36                              ; DW_AT_count
	.byte	0                               ; End Of Children Mark
	.byte	5                               ; Abbrev [5] 0x3b:0x4 DW_TAG_base_type
	.byte	5                               ; DW_AT_name
	.byte	6                               ; DW_AT_encoding
	.byte	1                               ; DW_AT_byte_size
	.byte	6                               ; Abbrev [6] 0x3f:0x4 DW_TAG_base_type
	.byte	6                               ; DW_AT_name
	.byte	8                               ; DW_AT_byte_size
	.byte	7                               ; DW_AT_encoding
	.byte	7                               ; Abbrev [7] 0x43:0x31 DW_TAG_subprogram
	.byte	1                               ; DW_AT_low_pc
.set Lset6, Lfunc_end0-Lfunc_begin0     ; DW_AT_high_pc
	.long	Lset6
	.byte	1                               ; DW_AT_frame_base
	.byte	109
	.byte	7                               ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	6                               ; DW_AT_decl_line
                                        ; DW_AT_prototyped
	.long	311                             ; DW_AT_type
                                        ; DW_AT_external
	.byte	8                               ; Abbrev [8] 0x52:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	126
	.byte	14                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	6                               ; DW_AT_decl_line
	.long	311                             ; DW_AT_type
	.byte	8                               ; Abbrev [8] 0x5d:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	125
	.byte	13                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	6                               ; DW_AT_decl_line
	.long	311                             ; DW_AT_type
	.byte	9                               ; Abbrev [9] 0x68:0xb DW_TAG_variable
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	127
	.byte	11                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	7                               ; DW_AT_decl_line
	.long	311                             ; DW_AT_type
	.byte	0                               ; End Of Children Mark
	.byte	10                              ; Abbrev [10] 0x74:0x31 DW_TAG_subprogram
	.byte	2                               ; DW_AT_low_pc
.set Lset7, Lfunc_end1-Lfunc_begin1     ; DW_AT_high_pc
	.long	Lset7
                                        ; DW_AT_APPLE_omit_frame_ptr
	.byte	1                               ; DW_AT_frame_base
	.byte	111
	.byte	10                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	15                              ; DW_AT_decl_line
                                        ; DW_AT_prototyped
	.long	333                             ; DW_AT_type
                                        ; DW_AT_external
	.byte	8                               ; Abbrev [8] 0x83:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	8
	.byte	14                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	15                              ; DW_AT_decl_line
	.long	333                             ; DW_AT_type
	.byte	8                               ; Abbrev [8] 0x8e:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	4
	.byte	13                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	15                              ; DW_AT_decl_line
	.long	333                             ; DW_AT_type
	.byte	9                               ; Abbrev [9] 0x99:0xb DW_TAG_variable
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	12
	.byte	11                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	16                              ; DW_AT_decl_line
	.long	333                             ; DW_AT_type
	.byte	0                               ; End Of Children Mark
	.byte	10                              ; Abbrev [10] 0xa5:0x31 DW_TAG_subprogram
	.byte	3                               ; DW_AT_low_pc
.set Lset8, Lfunc_end2-Lfunc_begin2     ; DW_AT_high_pc
	.long	Lset8
                                        ; DW_AT_APPLE_omit_frame_ptr
	.byte	1                               ; DW_AT_frame_base
	.byte	111
	.byte	16                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	29                              ; DW_AT_decl_line
                                        ; DW_AT_prototyped
	.long	382                             ; DW_AT_type
                                        ; DW_AT_external
	.byte	8                               ; Abbrev [8] 0xb4:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	16
	.byte	14                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	29                              ; DW_AT_decl_line
	.long	382                             ; DW_AT_type
	.byte	8                               ; Abbrev [8] 0xbf:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	0
	.byte	13                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	29                              ; DW_AT_decl_line
	.long	382                             ; DW_AT_type
	.byte	9                               ; Abbrev [9] 0xca:0xb DW_TAG_variable
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	32
	.byte	11                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	30                              ; DW_AT_decl_line
	.long	382                             ; DW_AT_type
	.byte	0                               ; End Of Children Mark
	.byte	10                              ; Abbrev [10] 0xd6:0x33 DW_TAG_subprogram
	.byte	4                               ; DW_AT_low_pc
.set Lset9, Lfunc_end3-Lfunc_begin3     ; DW_AT_high_pc
	.long	Lset9
                                        ; DW_AT_APPLE_omit_frame_ptr
	.byte	1                               ; DW_AT_frame_base
	.byte	111
	.byte	20                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	41                              ; DW_AT_decl_line
                                        ; DW_AT_prototyped
	.long	425                             ; DW_AT_type
                                        ; DW_AT_external
	.byte	8                               ; Abbrev [8] 0xe5:0xc DW_TAG_formal_parameter
	.byte	3                               ; DW_AT_location
	.byte	145
	.byte	8
	.byte	6
	.byte	14                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	41                              ; DW_AT_decl_line
	.long	425                             ; DW_AT_type
	.byte	8                               ; Abbrev [8] 0xf1:0xc DW_TAG_formal_parameter
	.byte	3                               ; DW_AT_location
	.byte	145
	.byte	0
	.byte	6
	.byte	13                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	41                              ; DW_AT_decl_line
	.long	425                             ; DW_AT_type
	.byte	9                               ; Abbrev [9] 0xfd:0xb DW_TAG_variable
	.byte	2                               ; DW_AT_location
	.byte	121
	.byte	0
	.byte	11                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	42                              ; DW_AT_decl_line
	.long	425                             ; DW_AT_type
	.byte	0                               ; End Of Children Mark
	.byte	11                              ; Abbrev [11] 0x109:0x17 DW_TAG_subprogram
	.byte	5                               ; DW_AT_low_pc
.set Lset10, Lfunc_end4-Lfunc_begin4    ; DW_AT_high_pc
	.long	Lset10
                                        ; DW_AT_APPLE_omit_frame_ptr
	.byte	1                               ; DW_AT_frame_base
	.byte	111
	.byte	24                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	55                              ; DW_AT_decl_line
                                        ; DW_AT_prototyped
                                        ; DW_AT_external
	.byte	8                               ; Abbrev [8] 0x114:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	8
	.byte	26                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	55                              ; DW_AT_decl_line
	.long	456                             ; DW_AT_type
	.byte	0                               ; End Of Children Mark
	.byte	11                              ; Abbrev [11] 0x120:0x17 DW_TAG_subprogram
	.byte	6                               ; DW_AT_low_pc
.set Lset11, Lfunc_end5-Lfunc_begin5    ; DW_AT_high_pc
	.long	Lset11
                                        ; DW_AT_APPLE_omit_frame_ptr
	.byte	1                               ; DW_AT_frame_base
	.byte	111
	.byte	25                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	59                              ; DW_AT_decl_line
                                        ; DW_AT_prototyped
                                        ; DW_AT_external
	.byte	8                               ; Abbrev [8] 0x12b:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	0
	.byte	26                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	59                              ; DW_AT_decl_line
	.long	491                             ; DW_AT_type
	.byte	0                               ; End Of Children Mark
	.byte	12                              ; Abbrev [12] 0x137:0x8 DW_TAG_typedef
	.long	319                             ; DW_AT_type
	.byte	9                               ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	4                               ; DW_AT_decl_line
	.byte	13                              ; Abbrev [13] 0x13f:0xe DW_TAG_structure_type
	.byte	1                               ; DW_AT_byte_size
	.byte	0                               ; DW_AT_decl_file
	.byte	4                               ; DW_AT_decl_line
	.byte	14                              ; Abbrev [14] 0x143:0x9 DW_TAG_member
	.byte	8                               ; DW_AT_name
	.long	59                              ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	4                               ; DW_AT_decl_line
	.byte	0                               ; DW_AT_data_member_location
	.byte	0                               ; End Of Children Mark
	.byte	12                              ; Abbrev [12] 0x14d:0x8 DW_TAG_typedef
	.long	341                             ; DW_AT_type
	.byte	15                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	14                              ; DW_AT_decl_line
	.byte	13                              ; Abbrev [13] 0x155:0x29 DW_TAG_structure_type
	.byte	4                               ; DW_AT_byte_size
	.byte	0                               ; DW_AT_decl_file
	.byte	14                              ; DW_AT_decl_line
	.byte	14                              ; Abbrev [14] 0x159:0x9 DW_TAG_member
	.byte	11                              ; DW_AT_name
	.long	59                              ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	14                              ; DW_AT_decl_line
	.byte	0                               ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x162:0x9 DW_TAG_member
	.byte	12                              ; DW_AT_name
	.long	59                              ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	14                              ; DW_AT_decl_line
	.byte	1                               ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x16b:0x9 DW_TAG_member
	.byte	13                              ; DW_AT_name
	.long	59                              ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	14                              ; DW_AT_decl_line
	.byte	2                               ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x174:0x9 DW_TAG_member
	.byte	14                              ; DW_AT_name
	.long	59                              ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	14                              ; DW_AT_decl_line
	.byte	3                               ; DW_AT_data_member_location
	.byte	0                               ; End Of Children Mark
	.byte	12                              ; Abbrev [12] 0x17e:0x8 DW_TAG_typedef
	.long	390                             ; DW_AT_type
	.byte	19                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	28                              ; DW_AT_decl_line
	.byte	13                              ; Abbrev [13] 0x186:0x17 DW_TAG_structure_type
	.byte	16                              ; DW_AT_byte_size
	.byte	0                               ; DW_AT_decl_file
	.byte	28                              ; DW_AT_decl_line
	.byte	14                              ; Abbrev [14] 0x18a:0x9 DW_TAG_member
	.byte	14                              ; DW_AT_name
	.long	413                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	28                              ; DW_AT_decl_line
	.byte	0                               ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x193:0x9 DW_TAG_member
	.byte	13                              ; DW_AT_name
	.long	413                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	28                              ; DW_AT_decl_line
	.byte	8                               ; DW_AT_data_member_location
	.byte	0                               ; End Of Children Mark
	.byte	12                              ; Abbrev [12] 0x19d:0x8 DW_TAG_typedef
	.long	421                             ; DW_AT_type
	.byte	18                              ; DW_AT_name
	.byte	1                               ; DW_AT_decl_file
	.byte	31                              ; DW_AT_decl_line
	.byte	5                               ; Abbrev [5] 0x1a5:0x4 DW_TAG_base_type
	.byte	17                              ; DW_AT_name
	.byte	7                               ; DW_AT_encoding
	.byte	8                               ; DW_AT_byte_size
	.byte	12                              ; Abbrev [12] 0x1a9:0x8 DW_TAG_typedef
	.long	433                             ; DW_AT_type
	.byte	23                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	40                              ; DW_AT_decl_line
	.byte	13                              ; Abbrev [13] 0x1b1:0x17 DW_TAG_structure_type
	.byte	32                              ; DW_AT_byte_size
	.byte	0                               ; DW_AT_decl_file
	.byte	40                              ; DW_AT_decl_line
	.byte	14                              ; Abbrev [14] 0x1b5:0x9 DW_TAG_member
	.byte	21                              ; DW_AT_name
	.long	382                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	40                              ; DW_AT_decl_line
	.byte	0                               ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x1be:0x9 DW_TAG_member
	.byte	22                              ; DW_AT_name
	.long	382                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	40                              ; DW_AT_decl_line
	.byte	16                              ; DW_AT_data_member_location
	.byte	0                               ; End Of Children Mark
	.byte	12                              ; Abbrev [12] 0x1c8:0x8 DW_TAG_typedef
	.long	464                             ; DW_AT_type
	.byte	30                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	54                              ; DW_AT_decl_line
	.byte	13                              ; Abbrev [13] 0x1d0:0x17 DW_TAG_structure_type
	.byte	8                               ; DW_AT_byte_size
	.byte	0                               ; DW_AT_decl_file
	.byte	54                              ; DW_AT_decl_line
	.byte	14                              ; Abbrev [14] 0x1d4:0x9 DW_TAG_member
	.byte	27                              ; DW_AT_name
	.long	487                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	54                              ; DW_AT_decl_line
	.byte	0                               ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x1dd:0x9 DW_TAG_member
	.byte	29                              ; DW_AT_name
	.long	487                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	54                              ; DW_AT_decl_line
	.byte	4                               ; DW_AT_data_member_location
	.byte	0                               ; End Of Children Mark
	.byte	5                               ; Abbrev [5] 0x1e7:0x4 DW_TAG_base_type
	.byte	28                              ; DW_AT_name
	.byte	4                               ; DW_AT_encoding
	.byte	4                               ; DW_AT_byte_size
	.byte	12                              ; Abbrev [12] 0x1eb:0x8 DW_TAG_typedef
	.long	499                             ; DW_AT_type
	.byte	34                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	58                              ; DW_AT_decl_line
	.byte	13                              ; Abbrev [13] 0x1f3:0x29 DW_TAG_structure_type
	.byte	32                              ; DW_AT_byte_size
	.byte	0                               ; DW_AT_decl_file
	.byte	58                              ; DW_AT_decl_line
	.byte	14                              ; Abbrev [14] 0x1f7:0x9 DW_TAG_member
	.byte	27                              ; DW_AT_name
	.long	540                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	58                              ; DW_AT_decl_line
	.byte	0                               ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x200:0x9 DW_TAG_member
	.byte	29                              ; DW_AT_name
	.long	540                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	58                              ; DW_AT_decl_line
	.byte	8                               ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x209:0x9 DW_TAG_member
	.byte	32                              ; DW_AT_name
	.long	540                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	58                              ; DW_AT_decl_line
	.byte	16                              ; DW_AT_data_member_location
	.byte	14                              ; Abbrev [14] 0x212:0x9 DW_TAG_member
	.byte	33                              ; DW_AT_name
	.long	540                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	58                              ; DW_AT_decl_line
	.byte	24                              ; DW_AT_data_member_location
	.byte	0                               ; End Of Children Mark
	.byte	5                               ; Abbrev [5] 0x21c:0x4 DW_TAG_base_type
	.byte	31                              ; DW_AT_name
	.byte	4                               ; DW_AT_encoding
	.byte	8                               ; DW_AT_byte_size
	.byte	0                               ; End Of Children Mark
Ldebug_info_end0:
	.section	__DWARF,__debug_str_offs,regular,debug
Lsection_str_off:
	.long	144                             ; Length of String Offsets Set
	.short	5
	.short	0
Lstr_offsets_base0:
	.section	__DWARF,__debug_str,regular,debug
Linfo_string:
	.asciz	"Apple clang version 17.0.0 (clang-1700.3.19.1)" ; string offset=0
	.asciz	"test_src/modules_test/libs/foo.c" ; string offset=47
	.asciz	"/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk" ; string offset=80
	.asciz	"MacOSX.sdk"                    ; string offset=132
	.asciz	"/Users/knix/dev/k1"            ; string offset=143
	.asciz	"char"                          ; string offset=162
	.asciz	"__ARRAY_SIZE_TYPE__"           ; string offset=167
	.asciz	"very_small"                    ; string offset=187
	.asciz	"small"                         ; string offset=198
	.asciz	"medium"                        ; string offset=204
	.asciz	"large"                         ; string offset=211
	.asciz	"hfa_small"                     ; string offset=217
	.asciz	"hfa_big"                       ; string offset=227
	.asciz	"VerySmall"                     ; string offset=235
	.asciz	"x"                             ; string offset=245
	.asciz	"Small"                         ; string offset=247
	.asciz	"r"                             ; string offset=253
	.asciz	"g"                             ; string offset=255
	.asciz	"b"                             ; string offset=257
	.asciz	"a"                             ; string offset=259
	.asciz	"Medium"                        ; string offset=261
	.asciz	"uint64_t"                      ; string offset=268
	.asciz	"unsigned long long"            ; string offset=277
	.asciz	"Large"                         ; string offset=296
	.asciz	"med1"                          ; string offset=302
	.asciz	"med2"                          ; string offset=307
	.asciz	"hfa1"                          ; string offset=312
	.asciz	"HFASmall"                      ; string offset=317
	.asciz	"f1"                            ; string offset=326
	.asciz	"float"                         ; string offset=329
	.asciz	"f2"                            ; string offset=335
	.asciz	"HFABig"                        ; string offset=338
	.asciz	"double"                        ; string offset=345
	.asciz	"f3"                            ; string offset=352
	.asciz	"f4"                            ; string offset=355
	.section	__DWARF,__debug_str_offs,regular,debug
	.long	0
	.long	47
	.long	80
	.long	132
	.long	143
	.long	162
	.long	167
	.long	187
	.long	245
	.long	235
	.long	198
	.long	253
	.long	255
	.long	257
	.long	259
	.long	247
	.long	204
	.long	277
	.long	268
	.long	261
	.long	211
	.long	302
	.long	307
	.long	296
	.long	217
	.long	227
	.long	312
	.long	326
	.long	329
	.long	335
	.long	317
	.long	345
	.long	352
	.long	355
	.long	338
	.section	__DWARF,__debug_addr,regular,debug
Lsection_info0:
.set Lset12, Ldebug_addr_end0-Ldebug_addr_start0 ; Length of contribution
	.long	Lset12
Ldebug_addr_start0:
	.short	5                               ; DWARF version number
	.byte	8                               ; Address size
	.byte	0                               ; Segment selector size
Laddr_table_base0:
	.quad	l_.str
	.quad	Lfunc_begin0
	.quad	Lfunc_begin1
	.quad	Lfunc_begin2
	.quad	Lfunc_begin3
	.quad	Lfunc_begin4
	.quad	Lfunc_begin5
Ldebug_addr_end0:
	.section	__DWARF,__debug_names,regular,debug
Ldebug_names_begin:
.set Lset13, Lnames_end0-Lnames_start0  ; Header: unit length
	.long	Lset13
Lnames_start0:
	.short	5                               ; Header: version
	.short	0                               ; Header: padding
	.long	1                               ; Header: compilation unit count
	.long	0                               ; Header: local type unit count
	.long	0                               ; Header: foreign type unit count
	.long	15                              ; Header: bucket count
	.long	18                              ; Header: name count
.set Lset14, Lnames_abbrev_end0-Lnames_abbrev_start0 ; Header: abbreviation table size
	.long	Lset14
	.long	8                               ; Header: augmentation string size
	.ascii	"LLVM0700"                      ; Header: augmentation string
.set Lset15, Lcu_begin0-Lsection_info   ; Compilation unit 0
	.long	Lset15
	.long	0                               ; Bucket 0
	.long	0                               ; Bucket 1
	.long	1                               ; Bucket 2
	.long	0                               ; Bucket 3
	.long	3                               ; Bucket 4
	.long	0                               ; Bucket 5
	.long	0                               ; Bucket 6
	.long	4                               ; Bucket 7
	.long	8                               ; Bucket 8
	.long	0                               ; Bucket 9
	.long	12                              ; Bucket 10
	.long	0                               ; Bucket 11
	.long	13                              ; Bucket 12
	.long	14                              ; Bucket 13
	.long	16                              ; Bucket 14
	.long	274559582                       ; Hash in Bucket 2
	.long	274559582                       ; Hash in Bucket 2
	.long	1046973349                      ; Hash in Bucket 4
	.long	214487437                       ; Hash in Bucket 7
	.long	265845232                       ; Hash in Bucket 7
	.long	265845232                       ; Hash in Bucket 7
	.long	-2060098749                     ; Hash in Bucket 7
	.long	31729958                        ; Hash in Bucket 8
	.long	217009403                       ; Hash in Bucket 8
	.long	226336838                       ; Hash in Bucket 8
	.long	226336838                       ; Hash in Bucket 8
	.long	857652610                       ; Hash in Bucket 10
	.long	1132417572                      ; Hash in Bucket 12
	.long	259121563                       ; Hash in Bucket 13
	.long	-113419488                      ; Hash in Bucket 13
	.long	290821634                       ; Hash in Bucket 14
	.long	2007945164                      ; Hash in Bucket 14
	.long	2090147939                      ; Hash in Bucket 14
	.long	198                             ; String in Bucket 2: small
	.long	247                             ; String in Bucket 2: Small
	.long	227                             ; String in Bucket 4: hfa_big
	.long	317                             ; String in Bucket 7: HFASmall
	.long	211                             ; String in Bucket 7: large
	.long	296                             ; String in Bucket 7: Large
	.long	187                             ; String in Bucket 7: very_small
	.long	338                             ; String in Bucket 8: HFABig
	.long	167                             ; String in Bucket 8: __ARRAY_SIZE_TYPE__
	.long	204                             ; String in Bucket 8: medium
	.long	261                             ; String in Bucket 8: Medium
	.long	277                             ; String in Bucket 10: unsigned long long
	.long	235                             ; String in Bucket 12: VerySmall
	.long	329                             ; String in Bucket 13: float
	.long	345                             ; String in Bucket 13: double
	.long	268                             ; String in Bucket 14: uint64_t
	.long	217                             ; String in Bucket 14: hfa_small
	.long	162                             ; String in Bucket 14: char
.set Lset16, Lnames3-Lnames_entries0    ; Offset in Bucket 2
	.long	Lset16
.set Lset17, Lnames9-Lnames_entries0    ; Offset in Bucket 2
	.long	Lset17
.set Lset18, Lnames7-Lnames_entries0    ; Offset in Bucket 4
	.long	Lset18
.set Lset19, Lnames14-Lnames_entries0   ; Offset in Bucket 7
	.long	Lset19
.set Lset20, Lnames5-Lnames_entries0    ; Offset in Bucket 7
	.long	Lset20
.set Lset21, Lnames13-Lnames_entries0   ; Offset in Bucket 7
	.long	Lset21
.set Lset22, Lnames2-Lnames_entries0    ; Offset in Bucket 7
	.long	Lset22
.set Lset23, Lnames16-Lnames_entries0   ; Offset in Bucket 8
	.long	Lset23
.set Lset24, Lnames1-Lnames_entries0    ; Offset in Bucket 8
	.long	Lset24
.set Lset25, Lnames4-Lnames_entries0    ; Offset in Bucket 8
	.long	Lset25
.set Lset26, Lnames10-Lnames_entries0   ; Offset in Bucket 8
	.long	Lset26
.set Lset27, Lnames12-Lnames_entries0   ; Offset in Bucket 10
	.long	Lset27
.set Lset28, Lnames8-Lnames_entries0    ; Offset in Bucket 12
	.long	Lset28
.set Lset29, Lnames15-Lnames_entries0   ; Offset in Bucket 13
	.long	Lset29
.set Lset30, Lnames17-Lnames_entries0   ; Offset in Bucket 13
	.long	Lset30
.set Lset31, Lnames11-Lnames_entries0   ; Offset in Bucket 14
	.long	Lset31
.set Lset32, Lnames6-Lnames_entries0    ; Offset in Bucket 14
	.long	Lset32
.set Lset33, Lnames0-Lnames_entries0    ; Offset in Bucket 14
	.long	Lset33
Lnames_abbrev_start0:
	.byte	1                               ; Abbrev code
	.byte	46                              ; DW_TAG_subprogram
	.byte	3                               ; DW_IDX_die_offset
	.byte	19                              ; DW_FORM_ref4
	.byte	4                               ; DW_IDX_parent
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; End of abbrev
	.byte	0                               ; End of abbrev
	.byte	2                               ; Abbrev code
	.byte	22                              ; DW_TAG_typedef
	.byte	3                               ; DW_IDX_die_offset
	.byte	19                              ; DW_FORM_ref4
	.byte	4                               ; DW_IDX_parent
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; End of abbrev
	.byte	0                               ; End of abbrev
	.byte	3                               ; Abbrev code
	.byte	36                              ; DW_TAG_base_type
	.byte	3                               ; DW_IDX_die_offset
	.byte	19                              ; DW_FORM_ref4
	.byte	4                               ; DW_IDX_parent
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; End of abbrev
	.byte	0                               ; End of abbrev
	.byte	0                               ; End of abbrev list
Lnames_abbrev_end0:
Lnames_entries0:
Lnames3:
L16:
	.byte	1                               ; Abbreviation code
	.long	116                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: small
Lnames9:
L14:
	.byte	2                               ; Abbreviation code
	.long	333                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: Small
Lnames7:
L11:
	.byte	1                               ; Abbreviation code
	.long	288                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: hfa_big
Lnames14:
L10:
	.byte	2                               ; Abbreviation code
	.long	456                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: HFASmall
Lnames5:
L9:
	.byte	1                               ; Abbreviation code
	.long	214                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: large
Lnames13:
L17:
	.byte	2                               ; Abbreviation code
	.long	425                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: Large
Lnames2:
L5:
	.byte	1                               ; Abbreviation code
	.long	67                              ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: very_small
Lnames16:
L15:
	.byte	2                               ; Abbreviation code
	.long	491                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: HFABig
Lnames1:
L12:
	.byte	3                               ; Abbreviation code
	.long	63                              ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: __ARRAY_SIZE_TYPE__
Lnames4:
L6:
	.byte	1                               ; Abbreviation code
	.long	165                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: medium
Lnames10:
L0:
	.byte	2                               ; Abbreviation code
	.long	382                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: Medium
Lnames12:
L7:
	.byte	3                               ; Abbreviation code
	.long	421                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: unsigned long long
Lnames8:
L8:
	.byte	2                               ; Abbreviation code
	.long	311                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: VerySmall
Lnames15:
L3:
	.byte	3                               ; Abbreviation code
	.long	487                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: float
Lnames17:
L2:
	.byte	3                               ; Abbreviation code
	.long	540                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: double
Lnames11:
L4:
	.byte	2                               ; Abbreviation code
	.long	413                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: uint64_t
Lnames6:
L1:
	.byte	1                               ; Abbreviation code
	.long	265                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: hfa_small
Lnames0:
L13:
	.byte	3                               ; Abbreviation code
	.long	59                              ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: char
	.p2align	2, 0x0
Lnames_end0:
.subsections_via_symbols
	.section	__DWARF,__debug_line,regular,debug
Lsection_line:
Lline_table_start0:

	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0	sdk_version 15, 5
	.globl	_clear_red                      ; -- Begin function clear_red
	.p2align	2
_clear_red:                             ; @clear_red
Lfunc_begin0:
	.file	0 "/Users/knix/dev/k1" "resources/c/struct.c" md5 0xa25377ad376094477c1a8758c9321987
	.loc	0 5 0                           ; resources/c/struct.c:5:0
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	mov	x8, x0
	str	w8, [sp, #8]
Ltmp1:
	.loc	0 6 11 prologue_end             ; resources/c/struct.c:6:11
	strb	wzr, [sp, #8]
	.loc	0 7 10                          ; resources/c/struct.c:7:10
	ldr	w8, [sp, #8]
	str	w8, [sp, #12]
	.loc	0 7 3 is_stmt 0                 ; resources/c/struct.c:7:3
	ldr	w0, [sp, #12]
	.loc	0 7 3 epilogue_begin            ; resources/c/struct.c:7:3
	add	sp, sp, #16
	ret
Ltmp2:
Lfunc_end0:
	.cfi_endproc
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
Lfunc_begin1:
	.loc	0 10 0 is_stmt 1                ; resources/c/struct.c:10:0
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	mov	w0, #0                          ; =0x0
	str	wzr, [sp, #12]
Ltmp4:
	.loc	0 11 3 prologue_end epilogue_begin ; resources/c/struct.c:11:3
	add	sp, sp, #16
	ret
Ltmp5:
Lfunc_end1:
	.cfi_endproc
                                        ; -- End function
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
	.byte	3                               ; Abbreviation Code
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
	.byte	4                               ; Abbreviation Code
	.byte	46                              ; DW_TAG_subprogram
	.byte	0                               ; DW_CHILDREN_no
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
	.byte	5                               ; Abbreviation Code
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
	.byte	6                               ; Abbreviation Code
	.byte	19                              ; DW_TAG_structure_type
	.byte	1                               ; DW_CHILDREN_yes
	.byte	3                               ; DW_AT_name
	.byte	37                              ; DW_FORM_strx1
	.byte	11                              ; DW_AT_byte_size
	.byte	11                              ; DW_FORM_data1
	.byte	58                              ; DW_AT_decl_file
	.byte	11                              ; DW_FORM_data1
	.byte	59                              ; DW_AT_decl_line
	.byte	11                              ; DW_FORM_data1
	.byte	0                               ; EOM(1)
	.byte	0                               ; EOM(2)
	.byte	7                               ; Abbreviation Code
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
	.byte	8                               ; Abbreviation Code
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
	.byte	1                               ; Abbrev [1] 0xc:0x7e DW_TAG_compile_unit
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
	.byte	0                               ; DW_AT_low_pc
.set Lset4, Lfunc_end1-Lfunc_begin0     ; DW_AT_high_pc
	.long	Lset4
.set Lset5, Laddr_table_base0-Lsection_info0 ; DW_AT_addr_base
	.long	Lset5
	.byte	2                               ; Abbrev [2] 0x25:0x1b DW_TAG_subprogram
	.byte	0                               ; DW_AT_low_pc
.set Lset6, Lfunc_end0-Lfunc_begin0     ; DW_AT_high_pc
	.long	Lset6
                                        ; DW_AT_APPLE_omit_frame_ptr
	.byte	1                               ; DW_AT_frame_base
	.byte	111
	.byte	5                               ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	5                               ; DW_AT_decl_line
                                        ; DW_AT_prototyped
	.long	79                              ; DW_AT_type
                                        ; DW_AT_external
	.byte	3                               ; Abbrev [3] 0x34:0xb DW_TAG_formal_parameter
	.byte	2                               ; DW_AT_location
	.byte	145
	.byte	8
	.byte	14                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	5                               ; DW_AT_decl_line
	.long	79                              ; DW_AT_type
	.byte	0                               ; End Of Children Mark
	.byte	4                               ; Abbrev [4] 0x40:0xf DW_TAG_subprogram
	.byte	1                               ; DW_AT_low_pc
.set Lset7, Lfunc_end1-Lfunc_begin1     ; DW_AT_high_pc
	.long	Lset7
                                        ; DW_AT_APPLE_omit_frame_ptr
	.byte	1                               ; DW_AT_frame_base
	.byte	111
	.byte	12                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	10                              ; DW_AT_decl_line
                                        ; DW_AT_prototyped
	.long	133                             ; DW_AT_type
                                        ; DW_AT_external
	.byte	5                               ; Abbrev [5] 0x4f:0x8 DW_TAG_typedef
	.long	87                              ; DW_AT_type
	.byte	11                              ; DW_AT_name
	.byte	0                               ; DW_AT_decl_file
	.byte	3                               ; DW_AT_decl_line
	.byte	6                               ; Abbrev [6] 0x57:0x2a DW_TAG_structure_type
	.byte	11                              ; DW_AT_name
	.byte	4                               ; DW_AT_byte_size
	.byte	0                               ; DW_AT_decl_file
	.byte	1                               ; DW_AT_decl_line
	.byte	7                               ; Abbrev [7] 0x5c:0x9 DW_TAG_member
	.byte	6                               ; DW_AT_name
	.long	129                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	2                               ; DW_AT_decl_line
	.byte	0                               ; DW_AT_data_member_location
	.byte	7                               ; Abbrev [7] 0x65:0x9 DW_TAG_member
	.byte	8                               ; DW_AT_name
	.long	129                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	2                               ; DW_AT_decl_line
	.byte	1                               ; DW_AT_data_member_location
	.byte	7                               ; Abbrev [7] 0x6e:0x9 DW_TAG_member
	.byte	9                               ; DW_AT_name
	.long	129                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	2                               ; DW_AT_decl_line
	.byte	2                               ; DW_AT_data_member_location
	.byte	7                               ; Abbrev [7] 0x77:0x9 DW_TAG_member
	.byte	10                              ; DW_AT_name
	.long	129                             ; DW_AT_type
	.byte	0                               ; DW_AT_decl_file
	.byte	2                               ; DW_AT_decl_line
	.byte	3                               ; DW_AT_data_member_location
	.byte	0                               ; End Of Children Mark
	.byte	8                               ; Abbrev [8] 0x81:0x4 DW_TAG_base_type
	.byte	7                               ; DW_AT_name
	.byte	6                               ; DW_AT_encoding
	.byte	1                               ; DW_AT_byte_size
	.byte	8                               ; Abbrev [8] 0x85:0x4 DW_TAG_base_type
	.byte	13                              ; DW_AT_name
	.byte	5                               ; DW_AT_encoding
	.byte	4                               ; DW_AT_byte_size
	.byte	0                               ; End Of Children Mark
Ldebug_info_end0:
	.section	__DWARF,__debug_str_offs,regular,debug
Lsection_str_off:
	.long	64                              ; Length of String Offsets Set
	.short	5
	.short	0
Lstr_offsets_base0:
	.section	__DWARF,__debug_str,regular,debug
Linfo_string:
	.asciz	"Apple clang version 17.0.0 (clang-1700.0.13.5)" ; string offset=0
	.asciz	"resources/c/struct.c"          ; string offset=47
	.asciz	"/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk" ; string offset=68
	.asciz	"MacOSX.sdk"                    ; string offset=120
	.asciz	"/Users/knix/dev/k1"            ; string offset=131
	.asciz	"clear_red"                     ; string offset=150
	.asciz	"main"                          ; string offset=160
	.asciz	"Color"                         ; string offset=165
	.asciz	"r"                             ; string offset=171
	.asciz	"char"                          ; string offset=173
	.asciz	"g"                             ; string offset=178
	.asciz	"b"                             ; string offset=180
	.asciz	"a"                             ; string offset=182
	.asciz	"int"                           ; string offset=184
	.asciz	"color"                         ; string offset=188
	.section	__DWARF,__debug_str_offs,regular,debug
	.long	0
	.long	47
	.long	68
	.long	120
	.long	131
	.long	150
	.long	171
	.long	173
	.long	178
	.long	180
	.long	182
	.long	165
	.long	160
	.long	184
	.long	188
	.section	__DWARF,__debug_addr,regular,debug
Lsection_info0:
.set Lset8, Ldebug_addr_end0-Ldebug_addr_start0 ; Length of contribution
	.long	Lset8
Ldebug_addr_start0:
	.short	5                               ; DWARF version number
	.byte	8                               ; Address size
	.byte	0                               ; Segment selector size
Laddr_table_base0:
	.quad	Lfunc_begin0
	.quad	Lfunc_begin1
Ldebug_addr_end0:
	.section	__DWARF,__debug_names,regular,debug
Ldebug_names_begin:
.set Lset9, Lnames_end0-Lnames_start0   ; Header: unit length
	.long	Lset9
Lnames_start0:
	.short	5                               ; Header: version
	.short	0                               ; Header: padding
	.long	1                               ; Header: compilation unit count
	.long	0                               ; Header: local type unit count
	.long	0                               ; Header: foreign type unit count
	.long	5                               ; Header: bucket count
	.long	5                               ; Header: name count
.set Lset10, Lnames_abbrev_end0-Lnames_abbrev_start0 ; Header: abbreviation table size
	.long	Lset10
	.long	8                               ; Header: augmentation string size
	.ascii	"LLVM0700"                      ; Header: augmentation string
.set Lset11, Lcu_begin0-Lsection_info   ; Compilation unit 0
	.long	Lset11
	.long	1                               ; Bucket 0
	.long	2                               ; Bucket 1
	.long	0                               ; Bucket 2
	.long	3                               ; Bucket 3
	.long	4                               ; Bucket 4
	.long	-213557786                      ; Hash in Bucket 0
	.long	2090499946                      ; Hash in Bucket 1
	.long	193495088                       ; Hash in Bucket 3
	.long	255668804                       ; Hash in Bucket 4
	.long	2090147939                      ; Hash in Bucket 4
	.long	150                             ; String in Bucket 0: clear_red
	.long	160                             ; String in Bucket 1: main
	.long	184                             ; String in Bucket 3: int
	.long	165                             ; String in Bucket 4: Color
	.long	173                             ; String in Bucket 4: char
.set Lset12, Lnames0-Lnames_entries0    ; Offset in Bucket 0
	.long	Lset12
.set Lset13, Lnames1-Lnames_entries0    ; Offset in Bucket 1
	.long	Lset13
.set Lset14, Lnames4-Lnames_entries0    ; Offset in Bucket 3
	.long	Lset14
.set Lset15, Lnames2-Lnames_entries0    ; Offset in Bucket 4
	.long	Lset15
.set Lset16, Lnames3-Lnames_entries0    ; Offset in Bucket 4
	.long	Lset16
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
	.byte	36                              ; DW_TAG_base_type
	.byte	3                               ; DW_IDX_die_offset
	.byte	19                              ; DW_FORM_ref4
	.byte	4                               ; DW_IDX_parent
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; End of abbrev
	.byte	0                               ; End of abbrev
	.byte	3                               ; Abbrev code
	.byte	22                              ; DW_TAG_typedef
	.byte	3                               ; DW_IDX_die_offset
	.byte	19                              ; DW_FORM_ref4
	.byte	4                               ; DW_IDX_parent
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; End of abbrev
	.byte	0                               ; End of abbrev
	.byte	4                               ; Abbrev code
	.byte	19                              ; DW_TAG_structure_type
	.byte	3                               ; DW_IDX_die_offset
	.byte	19                              ; DW_FORM_ref4
	.byte	4                               ; DW_IDX_parent
	.byte	25                              ; DW_FORM_flag_present
	.byte	0                               ; End of abbrev
	.byte	0                               ; End of abbrev
	.byte	0                               ; End of abbrev list
Lnames_abbrev_end0:
Lnames_entries0:
Lnames0:
L3:
	.byte	1                               ; Abbreviation code
	.long	37                              ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: clear_red
Lnames1:
L2:
	.byte	1                               ; Abbreviation code
	.long	64                              ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: main
Lnames4:
L0:
	.byte	2                               ; Abbreviation code
	.long	133                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: int
Lnames2:
L4:
	.byte	3                               ; Abbreviation code
	.long	79                              ; DW_IDX_die_offset
L1:                                     ; DW_IDX_parent
	.byte	4                               ; Abbreviation code
	.long	87                              ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: Color
Lnames3:
L5:
	.byte	2                               ; Abbreviation code
	.long	129                             ; DW_IDX_die_offset
	.byte	0                               ; DW_IDX_parent
                                        ; End of list: char
	.p2align	2, 0x0
Lnames_end0:
.subsections_via_symbols
	.section	__DWARF,__debug_line,regular,debug
Lsection_line:
Lline_table_start0:

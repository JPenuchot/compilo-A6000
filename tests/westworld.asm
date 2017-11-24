.text
	lw $a0, 0($a1)
	jal atoi
	sw $v0, 0($sp)
	addi $sp, $sp, -4
	jal main
	li $v0, 10
	syscall
main:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -4
#_main_0
	sw $zero, 0($sp)
	addi $sp, $sp, -4
	jal westworld
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	sw $t0, -4($fp)
#_main_1
	lw $t0, -4($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 40
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 24
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	jal play
	addi $sp, $sp, 12
	addi $sp, $sp, 4
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
bass:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -8
#_bass_0
	li $t0, 6
	li $t1, 4
	mul $a0, $t0, $t1
	li $v0, 9
	syscall
	sw $t0, 0($v0)
	sw $v0, -4($fp)
#_bass_1
	lw $t0, -4($fp)
	sw $t0, -8($fp)
#_bass_2
	li $t0, 0
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 0
	sw $t1, 0($t0)
#_bass_3
	li $t0, 1
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 2
	sw $t1, 0($t0)
#_bass_4
	li $t0, 2
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 7
	sw $t1, 0($t0)
#_bass_5
	li $t0, 3
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 14
	sw $t1, 0($t0)
#_bass_6
	li $t0, 4
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 15
	sw $t1, 0($t0)
#_bass_7
	li $t0, 5
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 15
	sw $t1, 0($t0)
#_bass_8
	lw $t0, -8($fp)
	sw $t0, 8($fp)
	addi $sp, $sp, 8
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
theme:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -8
#_theme_0
	li $t0, 12
	li $t1, 4
	mul $a0, $t0, $t1
	li $v0, 9
	syscall
	sw $t0, 0($v0)
	sw $v0, -4($fp)
#_theme_1
	lw $t0, -4($fp)
	sw $t0, -8($fp)
#_theme_2
	li $t0, 0
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 14
	sw $t1, 0($t0)
#_theme_3
	li $t0, 1
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 14
	sw $t1, 0($t0)
#_theme_4
	li $t0, 2
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 15
	sw $t1, 0($t0)
#_theme_5
	li $t0, 3
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 14
	sw $t1, 0($t0)
#_theme_6
	li $t0, 4
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 14
	sw $t1, 0($t0)
#_theme_7
	li $t0, 5
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 15
	sw $t1, 0($t0)
#_theme_8
	li $t0, 6
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 14
	sw $t1, 0($t0)
#_theme_9
	li $t0, 7
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 12
	sw $t1, 0($t0)
#_theme_10
	li $t0, 8
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 10
	sw $t1, 0($t0)
#_theme_11
	li $t0, 9
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 10
	sw $t1, 0($t0)
#_theme_12
	li $t0, 10
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 10
	sw $t1, 0($t0)
#_theme_13
	li $t0, 11
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -8($fp)
	add $t0, $t0, $t1
	li $t1, 10
	sw $t1, 0($t0)
#_theme_14
	lw $t0, -8($fp)
	sw $t0, 8($fp)
	addi $sp, $sp, 8
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
repeat:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -36
#_repeat_0
	li $t0, 2
	lw $t1, 8($fp)
	mul $t0, $t0, $t1
	sw $t0, -24($fp)
#_repeat_1
	lw $t0, -24($fp)
	li $t1, 4
	mul $a0, $t0, $t1
	li $v0, 9
	syscall
	sw $t0, 0($v0)
	sw $v0, -28($fp)
#_repeat_2
	lw $t0, -28($fp)
	sw $t0, -32($fp)
#_repeat_3
	li $t0, 0
	sw $t0, -36($fp)
#_repeat_4
#While
#_repeat_5
	b _label_repeat_2
#_label_repeat_1
_label_repeat_1:
#_repeat_7
#Corps de boucle
#_repeat_8
	lw $t0, -36($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, 12($fp)
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	sw $t0, -20($fp)
#_repeat_9
	lw $t0, -36($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -32($fp)
	add $t0, $t0, $t1
	lw $t1, -20($fp)
	sw $t1, 0($t0)
#_repeat_10
	lw $t0, -36($fp)
	lw $t1, 8($fp)
	add $t0, $t0, $t1
	sw $t0, -12($fp)
#_repeat_11
	lw $t0, -36($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, 12($fp)
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	sw $t0, -16($fp)
#_repeat_12
	lw $t0, -12($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -32($fp)
	add $t0, $t0, $t1
	lw $t1, -16($fp)
	sw $t1, 0($t0)
#_repeat_13
	lw $t0, -36($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -8($fp)
#_repeat_14
	lw $t0, -8($fp)
	sw $t0, -36($fp)
#_label_repeat_2
_label_repeat_2:
#_repeat_16
#Test de boucle
#_repeat_17
	lw $t0, -36($fp)
	lw $t1, 8($fp)
	slt $t0, $t0, $t1
	sw $t0, -4($fp)
#_repeat_18
	lw $t0, -4($fp)
	bnez $t0, _label_repeat_1
#_repeat_19
#Fin boucle
#_repeat_20
	lw $t0, 12($fp)
	sw $t0, 16($fp)
	addi $sp, $sp, 36
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
burn:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -48
#_burn_0
	li $t0, 0
	sw $t0, -48($fp)
#_burn_1
#While
#_burn_2
	b _label_burn_2
#_label_burn_1
_label_burn_1:
#_burn_4
#Corps de boucle
#_burn_5
	li $t0, 0
	sw $t0, -40($fp)
#_burn_6
	lw $t0, -48($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, 20($fp)
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	sw $t0, -32($fp)
#_burn_7
	lw $t0, -32($fp)
	lw $t1, 8($fp)
	add $t0, $t0, $t1
	sw $t0, -36($fp)
#_burn_8
	lw $t0, -36($fp)
	sw $t0, -44($fp)
#_burn_9
#While
#_burn_10
	b _label_burn_4
#_label_burn_3
_label_burn_3:
#_burn_12
#Corps de boucle
#_burn_13
	lw $t0, -48($fp)
	lw $t1, 12($fp)
	mul $t0, $t0, $t1
	sw $t0, -20($fp)
#_burn_14
	lw $t0, -20($fp)
	lw $t1, -40($fp)
	add $t0, $t0, $t1
	sw $t0, -24($fp)
#_burn_15
	lw $t0, -24($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, 24($fp)
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	sw $t0, -28($fp)
#_burn_16
	lw $t0, -44($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -28($fp)
	add $t0, $t0, $t1
	li $t1, -1
	sw $t1, 0($t0)
#_burn_17
	lw $t0, -40($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -16($fp)
#_burn_18
	lw $t0, -16($fp)
	sw $t0, -40($fp)
#_label_burn_4
_label_burn_4:
#_burn_20
#Test de boucle
#_burn_21
	lw $t0, -40($fp)
	lw $t1, 12($fp)
	slt $t0, $t0, $t1
	sw $t0, -12($fp)
#_burn_22
	lw $t0, -12($fp)
	bnez $t0, _label_burn_3
#_burn_23
#Fin boucle
#_burn_24
	lw $t0, -48($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -8($fp)
#_burn_25
	lw $t0, -8($fp)
	sw $t0, -48($fp)
#_label_burn_2
_label_burn_2:
#_burn_27
#Test de boucle
#_burn_28
	lw $t0, -48($fp)
	lw $t1, 16($fp)
	slt $t0, $t0, $t1
	sw $t0, -4($fp)
#_burn_29
	lw $t0, -4($fp)
	bnez $t0, _label_burn_1
#_burn_30
#Fin boucle
	addi $sp, $sp, 48
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
blank:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -40
#_blank_0
	lw $t0, 8($fp)
	li $t1, 4
	mul $a0, $t0, $t1
	li $v0, 9
	syscall
	sw $t0, 0($v0)
	sw $v0, -24($fp)
#_blank_1
	lw $t0, -24($fp)
	sw $t0, -28($fp)
#_blank_2
	li $t0, 0
	sw $t0, -32($fp)
#_blank_3
#While
#_blank_4
	b _label_blank_2
#_label_blank_1
_label_blank_1:
#_blank_6
#Corps de boucle
#_blank_7
	lw $t0, 12($fp)
	li $t1, 4
	mul $a0, $t0, $t1
	li $v0, 9
	syscall
	sw $t0, 0($v0)
	sw $v0, -20($fp)
#_blank_8
	lw $t0, -20($fp)
	sw $t0, -40($fp)
#_blank_9
	li $t0, 0
	sw $t0, -36($fp)
#_blank_10
#While
#_blank_11
	b _label_blank_4
#_label_blank_3
_label_blank_3:
#_blank_13
#Corps de boucle
#_blank_14
	lw $t0, -36($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -40($fp)
	add $t0, $t0, $t1
	li $t1, 0
	sw $t1, 0($t0)
#_blank_15
	lw $t0, -36($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -16($fp)
#_blank_16
	lw $t0, -16($fp)
	sw $t0, -36($fp)
#_label_blank_4
_label_blank_4:
#_blank_18
#Test de boucle
#_blank_19
	lw $t0, -36($fp)
	lw $t1, 12($fp)
	slt $t0, $t0, $t1
	sw $t0, -12($fp)
#_blank_20
	lw $t0, -12($fp)
	bnez $t0, _label_blank_3
#_blank_21
#Fin boucle
#_blank_22
	lw $t0, -32($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -28($fp)
	add $t0, $t0, $t1
	lw $t1, -40($fp)
	sw $t1, 0($t0)
#_blank_23
	lw $t0, -32($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -8($fp)
#_blank_24
	lw $t0, -8($fp)
	sw $t0, -32($fp)
#_label_blank_2
_label_blank_2:
#_blank_26
#Test de boucle
#_blank_27
	lw $t0, -32($fp)
	lw $t1, 8($fp)
	slt $t0, $t0, $t1
	sw $t0, -4($fp)
#_blank_28
	lw $t0, -4($fp)
	bnez $t0, _label_blank_1
#_blank_29
#Fin boucle
#_blank_30
	lw $t0, -28($fp)
	sw $t0, 16($fp)
	addi $sp, $sp, 40
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
westworld:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -28
#_westworld_0
	sw $zero, 0($sp)
	addi $sp, $sp, -4
	li $t0, 40
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 24
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	jal blank
	addi $sp, $sp, 8
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	sw $t0, -24($fp)
#_westworld_1
	lw $t0, -24($fp)
	sw $t0, -28($fp)
#_westworld_2
	sw $zero, 0($sp)
	addi $sp, $sp, -4
	jal theme
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	sw $t0, -20($fp)
#_westworld_3
	lw $t0, -28($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	lw $t0, -20($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 12
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 2
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 12
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	jal burn
	addi $sp, $sp, 20
#_westworld_4
	sw $zero, 0($sp)
	addi $sp, $sp, -4
	jal theme
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	sw $t0, -16($fp)
#_westworld_5
	lw $t0, -28($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	lw $t0, -16($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 12
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 2
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 24
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	jal burn
	addi $sp, $sp, 20
#_westworld_6
	sw $zero, 0($sp)
	addi $sp, $sp, -4
	jal bass
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	sw $t0, -4($fp)
#_westworld_7
	sw $zero, 0($sp)
	addi $sp, $sp, -4
	lw $t0, -4($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 6
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	jal repeat
	addi $sp, $sp, 8
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	sw $t0, -8($fp)
#_westworld_8
	sw $zero, 0($sp)
	addi $sp, $sp, -4
	lw $t0, -8($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 12
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	jal repeat
	addi $sp, $sp, 8
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	sw $t0, -12($fp)
#_westworld_9
	lw $t0, -28($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	lw $t0, -12($fp)
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 24
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 1
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	li $t0, 0
	sw $t0, 0($sp)
	addi $sp, $sp, -4
	jal burn
	addi $sp, $sp, 20
#_westworld_10
	lw $t0, -28($fp)
	sw $t0, 8($fp)
	addi $sp, $sp, 28
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
play:
	sw $fp, 0($sp)
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	addi $sp, $sp, -4
	addi $fp, $sp, 4
	addi $sp, $sp, -40
#_play_0
	lw $t0, 8($fp)
	li $t1, 1
	sub $t0, $t0, $t1
	sw $t0, -32($fp)
#_play_1
	lw $t0, -32($fp)
	sw $t0, -36($fp)
#_play_2
#While
#_play_3
	b _label_play_2
#_label_play_1
_label_play_1:
#_play_5
#Corps de boucle
#_play_6
	lw $t0, 12($fp)
	li $t1, 1
	sub $t0, $t0, $t1
	sw $t0, -28($fp)
#_play_7
	lw $t0, -28($fp)
	sw $t0, -40($fp)
#_play_8
#While
#_play_9
	b _label_play_4
#_label_play_3
_label_play_3:
#_play_11
#Corps de boucle
#_play_12
#If
#_play_13
	lw $t0, -36($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, 16($fp)
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	sw $t0, -20($fp)
#_play_14
	lw $t0, -40($fp)
	li $t1, 4
	mul $t0, $t0, $t1
	lw $t1, -20($fp)
	add $t0, $t0, $t1
	lw $t0, 0($t0)
	sw $t0, -24($fp)
#_play_15
	lw $t0, -24($fp)
	bnez $t0, _label_play_5
#_play_16
#Bloc else
#_play_17
	li $a0, 32
	li $v0, 11
	syscall
#_play_18
	li $a0, 32
	li $v0, 11
	syscall
#_play_19
	b _label_play_6
#_label_play_5
_label_play_5:
#_play_21
#Bloc then
#_play_22
	li $a0, 35
	li $v0, 11
	syscall
#_play_23
	li $a0, 35
	li $v0, 11
	syscall
#_label_play_6
_label_play_6:
#_play_25
#Fin if
#_play_26
	lw $t0, -40($fp)
	li $t1, 1
	sub $t0, $t0, $t1
	sw $t0, -16($fp)
#_play_27
	lw $t0, -16($fp)
	sw $t0, -40($fp)
#_label_play_4
_label_play_4:
#_play_29
#Test de boucle
#_play_30
	li $t0, 0
	lw $t1, -40($fp)
	sle $t0, $t0, $t1
	sw $t0, -12($fp)
#_play_31
	lw $t0, -12($fp)
	bnez $t0, _label_play_3
#_play_32
#Fin boucle
#_play_33
	li $a0, 10
	li $v0, 11
	syscall
#_play_34
	lw $t0, -36($fp)
	li $t1, 1
	sub $t0, $t0, $t1
	sw $t0, -8($fp)
#_play_35
	lw $t0, -8($fp)
	sw $t0, -36($fp)
#_label_play_2
_label_play_2:
#_play_37
#Test de boucle
#_play_38
	li $t0, 0
	lw $t1, -36($fp)
	sle $t0, $t0, $t1
	sw $t0, -4($fp)
#_play_39
	lw $t0, -4($fp)
	bnez $t0, _label_play_1
#_play_40
#Fin boucle
	addi $sp, $sp, 40
	addi $sp, $sp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	jr $ra
atoi:
	move $t0, $a0
	li $t1, 0
	li $t2, 10
atoi_loop:
	lbu $t3, 0($t0)
	beq $t3, $zero, atoi_end
	li $t4, 48
	blt $t3, $t4, atoi_error
	li $t4, 57
	bgt $t3, $t4, atoi_error
	addi $t3, $t3, -48
	mul $t1, $t1, $t2
	add $t1, $t1, $t3
	addi $t0, $t0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	move $v0, $t1
	jr $ra
.data

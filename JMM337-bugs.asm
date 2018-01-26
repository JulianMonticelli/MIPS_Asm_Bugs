#
# Written by Julian Monticelli
# University of Pittsburgh 2016
#

.data
hit: .asciiz "Hit down\n"
hitUp: .asciiz "Hit up\n"
endGameMessage: .asciiz "\nThe game score is "
colon: .asciiz " : "
newline: .asciiz "\n"
.text
j checkForStartInput


init:
	move $fp, $sp
	move $s5,$fp # $s5 will hold top of queue (stack)
	li $t5, 15 # load t5 with percentage per tick of bug spawning
	move $s6,$s5 # $s6 will hold beginning adress of last element in queue
	li $v1, 1
	jr $ra

main:
	jal init
	jal addBug # Guarantees at least one bug 0 childers said spawn from 1-4 on te beginning
	jal addBug
	# I decided that it was wise to guarantee one spawn and let 15% probability per tick begin the program
	jal mainLoop
	
mainLoop:
	# get time of game mainLoop execution
	li $v0, 30
	syscall
	move $s3,$a0 # higher order seconds into $s3
	move $s4,$a1 # milliseconds into $s4
	
	jal rollForBug
	# run queueLoop (goes through queue)
	jal queueStart
	jal drawPlayer
	jal checkForInput
	addi $t4, $t4, 1 # add 1 to counter (every 10 seconds / 100 ticks add 5% chance for spawn - on 1200 ticks, end program)
	jal checkTick
	jal timeStep
	j mainLoop
	
################################################
# Primary queue iteration methods              #
################################################

queueStart:
	move $s1,$ra # save $ra for queueLoop in $s1
	move $fp, $s5
    
queueLoop:
	lb $t7, 0($fp)
	li $t9, 'k'
	beq $t7, $t9, skip # skip kill events
	li $t9, 'b'
	lb $a0, 1($fp) # x to $a0
	lb $a1, 2($fp) # y to $a1
	jal savePastNextInstruction # $ra = $pc + 8
	beq $t7, $t9, bugMove # move the bug if it's a bug
	li $t9, 'p'
	jal savePastNextInstruction
	beq $t7, $t9, pulseMove # move the pulse if it's a pulse
	li $t9, 'w'
	lb $a3, 3($fp)
	jal savePastNextInstruction
	beq $t7, $t9, waveMove # move the wave if it's a wave
	####should be out of types at this point
    
terminateIteration:
	beq $fp, $s6, exitQueueLoop
	addi $fp, $fp, -4
	j queueLoop
    
exitQueueLoop:
	jr $s1
################################################

################################################
# Input checking section                       #
################################################
checkForStartInput: # loop looking for 'b' button
	la $v0, 0xFFFF0000
	lw $t0, 0($v0)
	andi $t0, $t0, 1
	beqz $t0, checkForStartInput
	lw $t0, 4($v0)
	addi $v0, $t0, -66
	bnez $v0, checkForStartInput
	j main
    
checkForInput:
	move $s1, $ra
	la $v0, 0xFFFF0000
	lw $t0, 0($v0)
	andi $t0, $t0, 1
	beqz $t0, return
	lw $t0, 4($v0)
	addi $v0, $t0, -226 # Check for left key
	bnez $v0, rkey
		move $a0, $v1
		li $a1, 0x3f
		li $a2, 0
		jal setLED
		addi $v1,$v1,-1
		j checkPlayer
        
	rkey:   addi $v0, $t0, -227 # check for right key
		bnez $v0, dkey
		move $a0, $v1
		li $a1, 0x3f
		li $a2, 0
		jal setLED
		addi $v1, $v1, 1
		j checkPlayer
        
	dkey:   addi $v0, $t0, -225 # check for down key
		bnez $v0, b_butt
		j endGame
        
	b_butt: addi $v0, $t0, -66
		bnez $v0, bkey
		j endGame
        
	bkey:   addi $v0, $t0, -224 # check for blaster key (up key)
		bnez $v0, return
		jal addPulse
        
return:
	jr $s1
################################################

checkPlayer:
	# to check player's position, I just masked the lower bits with 0x3f
	andi $v1, $v1, 0x3F
	j return

savePastNextInstruction:
	move $t8,$ra
	addi $ra,$ra,4
	jr $t8




# will trash $a0 and $a1

rollForBug:
	li $a1, 101 # number range 0-100
	li $a2, 682794651 # seed
	li $v0, 42 # random
	syscall
	blt $a0, $t5, addBug
	jr $ra
    
    
################################################
# Bug movement and hit detection methods       #
################################################

bugMove:
	# is this bug moving from 63 off the screen?
	li $t9, 0x3F # t9 to 63
	beq $a1, $t9, removeCurrentEntity
	# turn off old LED
	li $a2, 0
	jal setLED
	# turn on next LED
	li $a2, 3
	addi $a1, $a1, 1 # (y+1)
	sb $a1, 2($fp) # store (y+1) back into memory
	jal rarLED
	jal bugHitDetection
	j terminateIteration
	#jr $s0
    
bugHitDetection:
	move $sp, $s5 # sp is stack pointer for hitDetection
	li $t9, 1 # t9 is red (pulse color)
	beq $v0, $t9, bugHitDetectionLoop
	j terminateIteration
	#jr $s0
    
bugHitDetectionLoop:
	blt $sp,$s6,exitPulseLoop # did $sp pass up the end of tbe circular queue? exit this loop!
	lb $t7, 0($sp) #load $t7
	li $t9, 'k'
	beq $t7, $t9, skip_bug_detection # killed bug? skip it.
	li $t9, 'b'
	beq $t7, $t9, skip_bug_detection # live bug? skip this bug detection.
	li $t9, 'p'
	lb $t0, 1($sp)
	lb $t1, 2($sp)
	jal savePastNextInstruction # ra = pc + 8
	beq $t7, $t9, comparePulseBug
	li $t9, 'w'
	lb $a3, 3($sp)
	jal savePastNextInstruction
	beq $t7, $t9, compareWaveBug
	####stopped here
	addi $sp, $sp, -4
	j bugHitDetectionLoop
    
comparePulseBug:
	sub $t2,$t0,$a0
	beqz $t2, checkYForNegPB
	jr $ra
    
checkYForNegPB:
	sub $t3,$t1,$a1
	beqz $t3, bugToPulseWave
	jr $ra
    
bugToPulseWave:
	###########
	#move $a2, $a0
	#la $a0, hit
	#li $v0, 4
	#syscall
	#move $a0, $a2
	#move $a2, $0
	#######
	li $a2, 0
	jal setLED
	addi $s2, $s2, 1
	# bug event to kill event
	li $t9, 'k'
	sb $t9, 0($fp)
	li $t9, 'w'
	sb $t9, 0($sp)
	j terminateIteration
	#jr $s0
    
## begin comparing bug to wave locations
compareWaveBug:
	add $t0, $t0, $a3
	li $t8, 0x3F
	jal compareBugToWave
	sub $t1, $t1, $a3
	jal compareBugToWave
	add $t1, $t1, $a3 # middle right
	add $t1, $t1, $a3 # top right
	jal compareBugToWave
	sub $t0, $t0, $a3
	jal compareBugToWave
	sub $t0, $t0, $a3
	jal compareBugToWave
	sub $t1, $t1, $a3
	jal compareBugToWave
	sub $t1, $t1, $a3
	jal compareBugToWave
	add $t0, $t0, $a3
	jal compareBugToWave
	j terminateIteration
	#jr $s0
    
compareBugToWave:
	sub $t2,$a0,$t0
	beqz $t2, checkYForNegBW
	jr $ra
    
checkYForNegBW:
	sub $t3,$a1,$t1
	beqz $t3, bugPulseToWave
	jr $ra
    
bugPulseToWave:
	li $t9, 'w'
	sb $t9, 0($fp)
	addi $s2, $s2, 1
	j terminateIteration
	#jr $s0
    
################################################

################################################
# Pulse movement and hit detection methods     #
################################################

pulseMove:
	#move $s0, $ra # preserve return address
	# is this bug moving from 63 off the screen?
	beqz $a1, removeCurrentEntity
	# turn off old LED
	li $a2, 0
	jal setLED
	# turn on next LED
	li $a2, 1
	addi $a1, $a1, -1 # (y-1)
	sb $a1, 2($fp) # store (y-1) back into memory
	jal rarLED
	jal pulseHitDetection
	#li $a2,0
	j terminateIteration
	#jr $s0
    
pulseHitDetection:
	#a0, a1 are preserved
	beqz $a1, exitPulseLoop
	#addi $a1,$a1,-1
	#jal getLED
	li $a2,0 # just added
	move $sp, $s5 # sp is stack pointer for hitDetection
	li $t9, 3 # t9 is green
	beq $v0, $t9, pulseHitDetectionLoop
	j terminateIteration
	#jr $s0
    
pulseHitDetectionLoop:
	blt $sp,$s6,exitPulseLoop
	lb $t7, 0($sp)
	li $t9, 'k'
	beq $t7, $t9, skip_detection
	li $t9, 'b'
	lb $t0, 1($sp)
	lb $t1, 2($sp)
	jal savePastNextInstruction
	beq $t7, $t9, compareBugPulse
	####stopped here
	addi $sp, $sp, -4
	j pulseHitDetectionLoop
    
compareBugPulse:
	sub $t2,$t0,$a0
	beqz $t2, checkYForNegP
	jr $ra
    
checkYForNegP:
	sub $t3,$t1,$a1
	beqz $t3, pulseToWave
	jr $ra
    
pulseToWave:
	########### This text can be used to determine that up and down hits are being detected
	#move $a2, $a0
	#la $a0, hitUp
	#li $v0, 4
	#syscall
	#move $a0, $a2
	#move $a2, $0
	##########
	addi $s2, $s2, 1
	li $t9,'w'
	sb $t9, 0($fp)
	li $t9,'k'
	sb $t9, 0($sp)
	j terminateIteration
	#jr $s0
    
exitPulseLoop:
	j terminateIteration
	#jr $s0
################################################

################################################
# Wave movement and hit detection methods      #
################################################

waveMove:
	#move $s0, $ra
	# turn off old LEDs
	li $t9, 10
	li $t8, 0x3F # 63 in $t8 to compare limit of LED
	li $a2, 0
	add $a0, $a0, $a3 # right side of pulse
    
r_middleRight:
	# middle right is currently in $a0 and $a1...
	blt $t8, $a0, r_skipRight # too far right? skip the right completely.
	jal setLED
    
r_topRight:
	sub $a1, $a1, $a3
	bltz $a1, r_bottomRight # above LED display? check bottom right then.
	jal setLED
    
r_bottomRight:
	add $a1, $a1, $a3 # middle right
	add $a1, $a1, $a3 # bottom right
	blt $t8, $a1, r_skipBottom  # too far south of the LED? check the middle left then
	jal setLED
    
r_bottomMiddle:
	sub $a0, $a0, $a3
	# no check needed (middle is fine, bottom already checked)
	jal setLED
    
r_bottomLeft:
	sub $a0, $a0, $a3
	bltz $a0, r_skipLeft # too far left of LED? skip the left completely.
	jal setLED
    
r_middleLeft:
	sub $a1, $a1, $a3
	bltz $a0, r_skipLeft2
	jal setLED
    
r_topLeft:
	sub $a1, $a1, $a3
	bltz $a1, r_topMiddle
	jal setLED
    
r_topMiddle:
	add $a0, $a0, $a3
	bltz $a1, increment
	jal setLED
    
increment:
	beq $a3, $t9, killWave # all LED's removed, so should we increment?
	# back to center
	add $a1, $a1, $a3
	addi $a3, $a3, 1
	sb $a3, 3($fp) # store (range+1) back into memory
	add $a0, $a0, $a3 # back to middle right
	# turn on next LEDs (and check for conflicting bugs)
	li $a2, 1
    
middleRight:
	# middle right is currently in $a0 and $a1...
	blt $t8, $a0, skipRight # too far right? skip the right completely.
	jal rarLED # set and get the LED
	jal waveHitDetection
    
topRight:
	sub $a1, $a1, $a3
	bltz $a1, bottomRight # above LED display? check bottom right then.
	jal rarLED
	jal waveHitDetection
    
bottomRight:
	add $a1, $a1, $a3 # middle right
	add $a1, $a1, $a3 # top right
	blt $t8, $a1, skipBottom  # too far south of the LED? check the middle left then
	jal rarLED
	jal waveHitDetection
    
bottomMiddle:
	sub $a0, $a0, $a3
	# no check needed (middle is fine, bottom already checked)
	jal rarLED
	jal waveHitDetection
    
bottomLeft:
	sub $a0, $a0, $a3
	bltz $a0, skipLeft # too far left of LED? skip the left completely.
	jal rarLED
	jal waveHitDetection
    
middleLeft:
	sub $a1, $a1, $a3
	bltz $a0, skipLeft2 #skip left to prevent out of bounds
	jal rarLED
	jal waveHitDetection
    
topLeft:
	sub $a1, $a1, $a3
	bltz $a1, topMiddle #skip to top middle b/c too far left
	jal rarLED
	jal waveHitDetection
    
topMiddle:
	add $a0, $a0, $a3
	bltz $a1, endLoop #top middle too high? end the loop
	jal rarLED
	jal waveHitDetection
    
endLoop:
	j terminateIteration
	
	# skips below account for movement of wave "pointer" throughout skipped methods
r_skipRight:
	add $a1, $a1, $a3
	j r_bottomMiddle
    
r_skipBottom:
	sub $a0, $a0, $a3
	sub $a0, $a0, $a3
	j r_middleLeft
    
r_skipLeft:
	sub $a1, $a1, $a3
	sub $a1, $a1, $a3
	j r_topMiddle
    
r_skipLeft2:
	sub $a1, $a1, $a3
	j r_topMiddle
    
skipRight:
	add $a1, $a1, $a3
	j bottomMiddle
    
skipBottom:
	sub $a0, $a0, $a3
	sub $a0, $a0, $a3
	j middleLeft
    
skipLeft:
	sub $a1, $a1, $a3
	sub $a1, $a1, $a3
	j topMiddle
    
skipLeft2:
	sub $a1, $a1, $a3
	j topMiddle
    
waveHitDetection:
	move $t6, $ra
	move $sp, $s5 # sp is stack pointer for hitDetection
	li $t9, 3 # t9 is green
	beq $v0, $t9, waveHitDetectionLoop
	jr $t6
    
waveHitDetectionLoop:
	blt $sp,$s6,exitPulseLoop
	lb $t7, 0($sp)
	li $t9, 'k'
	beq $t7, $t9, skip_wave_detection
	li $t9, 'b'
	lb $t0, 1($sp)
	lb $t1, 2($sp)
	jal savePastNextInstruction
	beq $t7, $t9, compareBugWave
	####stopped here
	addi $sp, $sp, -4
	j waveHitDetectionLoop
    
compareBugWave:
	sub $t2,$t0,$a0
	beqz $t2, checkYForNegW
	jr $ra
    
checkYForNegW:
	sub $t3,$t1,$a1
	beqz $t3, killBugFromPulse
	jr $ra
    
killBugFromPulse:
	li $t9, 'w'
	sb $t9, 0($sp)
	addi $s2, $s2, 1
	jr $ra
    
killWave:
	li $t9, 'k'
	sb $t9, 0($fp)
	j terminateIteration
    
################################################

removeCurrentEntity:
	li $t9, 'k'
	sb $t9, 0($fp)
	li $a2, 0
	jal setLED
	j terminateIteration

addBug:
	li $a1, 64 # number range 0-63
	li $v0, 42 # random
	syscall # random x value generated
	li $a1, 0 # y set to 0
	addi $s6,$s6,-4 # adding a bug should also add an entry to the final queue entry
	li $t9,'b' # store 'b' (character that represents bug) in register $t9 (henceforth a highly volatile register)
	sb $t9, 0($s6) # store bug event in first byte
	sb $a0, 1($s6) # store x position in the second byte
	sb $a1, 2($s6) # store y position (0) in the third byte
	sb $0, 3($s6) # store a 0 in the final byte (0 would be used for the radius of the wave)
	jr $ra

#add
addPulse:
	move $s0, $ra
	addi $s7, $s7, 1
	addi $s6,$s6,-4 # adding a pulse should also add an entry to the final queue entry
	li $t9,'p' # store 'p' (character that represents pulse) in register $t9 (volatile storage0
	sb $t9, 0($s6) # 'p' in +0
	move $a0, $v1
	sb $a0, 1($s6) # value of x
	li $a1, 62
	sb $a1, 2($s6) # value of y
	sb $0, 3($s6)
	li $a2, 1
	jal setLED
	li $a2, 0
	jr $s0
    
drawPlayer:
	move $s1, $ra
	move $a0, $v1
	li $a1, 0x3F # Y: 63
	li $a2, 2
	jal setLED
	li $a2, 0
	jr $s1
###############################################
# Check time functions                        #
###############################################
timeStep:
	#move $s0, $ra
	li $v0, 30
	syscall # a0 and a1 contanin 64-bit time
	sub $a0, $a0, $s3
	sub $a1, $a1, $s4
	beqz $a1, checkLowOrderTime # bit 32 is unchanged, can compare low order bits
	# high order time changed? we don't want to wait >2^31 ms for it to roll back
	# instead of doing the subtraction, since this only happens once every week or so, you can just skip the time check if this happens
	jr $ra
    
checkLowOrderTime:
	li $t0, 100
	blt $a0, $t0, sleep
	jr $ra
    
sleep:
	sub $a0, $t0, $a1 # get ms time to sleep in a0 by subbing a1 and t0 (100)
	li $v0, 32 # sleep code
	syscall
	jr $ra

################################################
# Check tick functions                         #
################################################

checkTick:
	li $t9, 100
	beq $t4, $t9, addChance #100	
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #200
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #300
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #400
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #500
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #600
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #700
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #800
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #900
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #1000
	addi $t9, $t9, 100
	beq $t4, $t9, addChance #1100
	addi $t9, $t9, 100
	beq $t4, $t9, endGame #1200 - 2:00- end game
	jr $ra
    
addChance:
	addi $t5, $t5, 5 # increase % of adding a bug by 5% (maxes out at 70%
	jr $ra
################################################
# Queue iteration skip methods                 #
################################################
skip:
	beq $fp,$s6, exitQueueLoop
	addi $fp,$fp,-4
	j queueLoop
    
skip_detection:
	addi $sp,$sp,-4
	j pulseHitDetectionLoop
    
skip_bug_detection:
	addi $sp,$sp,-4
	j bugHitDetectionLoop
    
skip_wave_detection:
	addi $sp,$sp,-4
	j waveHitDetectionLoop
    
################################################

################################################
# LED manipulation methods                     #
################################################
setLED:
	#arguments: $a0 is x, $a1 is y, $a2 is color
	# byte offset into display = y * 16 byte + (x/4)
	sll $t0,$a1,4		# y * 16 bytes
	srl $t1,$a0,2		# x / 4
	add $t0,$t0,$t1		# byte offset into display
	li $t2, 0xffff0008	# base address of LED display
	add $t0,$t2,$t0		# address of byte with the LED
	# now, compute LED position in the byte and mask for it
	andi $t1, $a0, 0x3	# remainder is LED position in byte
	neg $t1,$t1		# negate position for subtraction
	addi $t1,$t1,3		# bit positions in reverse order
	sll $t1,$t1,1		# led is 2 bits
	# compute two masks: one to clear field, one to set new color
	li $t2,3
	sllv $t2,$t2,$t1
	not $t2,$t2		# bit mask for clearing current color
	sllv $t1,$a2,$t1	# bit mask for setting color
	# get current LED value, set the new field, store it back to LED
	lbu $t3,0($t0)		# read current LED value
	and $t3,$t3,$t2		# clear the field for the color
	or $t3,$t3,$t1		# set color field
	sb $t3,0($t0)		# update display
	jr $ra
    
getLED:
	# byte offset into display = y * 16 bytes + (x / 4)
	sll  $t0,$a1,4      # y * 16 bytes
	srl  $t1,$a0,2      # x / 4
	add  $t0,$t0,$t1    # byte offset into display
	la   $t2,0xffff0008
	add  $t0,$t2,$t0    # address of byte with the LED
	# now, compute bit position in the byte and the mask for it
	andi $t1,$a0,0x3    # remainder is bit position in byte
	neg  $t1,$t1        # negate position for subtraction
	addi $t1,$t1,3      # bit positions in reverse order
    	sll  $t1,$t1,1      # led is 2 bits
	# load LED value, get the desired bit in the loaded byte
	lbu  $t2,0($t0)
	srlv $t2,$t2,$t1    # shift LED value to lsb position
	andi $v0,$t2,0x3    # mask off any remaining upper bits
	jr   $ra

rarLED: # replace and return
	#arguments: $a0 is x, $a1 is y, $a2 is color
	# byte offset into display = y * 16 byte + (x/4)
	sll $t0,$a1,4		# y * 16 bytes
	srl $t1,$a0,2		# x / 4
	add $t0,$t0,$t1		# byte offset into display
	li $t2, 0xffff0008	# base address of LED display
	add $t0,$t2,$t0		# address of byte with the LED
	# now, compute LED position in the byte and mask for it
	andi $t1, $a0, 0x3	# remainder is LED position in byte
	neg $t1,$t1		# negate position for subtraction
	addi $t1,$t1,3		# bit positions in reverse order
	sll $t1,$t1,1		# led is 2 bits
	# load LED value, get the desired bit in the loaded byte
	lbu  $t2,0($t0)
	srlv $t2,$t2,$t1    # shift LED value to lsb position
	andi $v0,$t2,0x3    # mask off any remaining upper bits
	# compute two masks: one to clear field, one to set new color
	li $t2,3
	sllv $t2,$t2,$t1
	not $t2,$t2		# bit mask for clearing current color
	sllv $t1,$a2,$t1	# bit mask for setting color
	# get current LED value, set the new field, store it back to LED
	lbu $t3,0($t0)		# read current LED value
	and $t3,$t3,$t2		# clear the field for the color
	or $t3,$t3,$t1		# set color field
	sb $t3,0($t0)		# update display
	jr $ra
    
################################################

################################################
# Program Exit				       #
################################################


endGame:
	la $a0, endGameMessage
	li $v0, 4 # print message
	syscall
	li $v0, 1 # print int
	move $a0, $s2
	syscall
	la $a0, colon
	li $v0, 4 # print message
	syscall
	move $a0, $s7
	li $v0, 1 # print int
	syscall
	la $a0, newline
	li $v0, 4 # print message
	syscall
	j exit
    
exit:
	li $v0, 10
	syscall # exit

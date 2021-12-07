LDA #$00
STA $6100

LDA #$C7
STA $6101

LDA #$00
STA $6102

LDA #$C8
STA $6103

LDA #$00
STA $6104

LDA #$D0
STA $6105

LDA #$00
STA $6106

LDA #$D2
STA $6107

LDA #$00
STA $6108

LDA #$C8
STA $6109

LDA #$00
STA $610A

LDA #$CF
STA $610B

LDA #$00
STA $610C

LDA #$F0
STA $610D

LDA #$01
STA $610E

LDA #$0D
STA $610F

LDA #$01
STA $6110

LDA #$04
STA $6111

LDA #$01
STA $6112

LDA #$07
STA $6113

;ADR2_LOW  = $6100 ; this is a static location, but what I really need is to be able to increment this
;ADR2_HIGH = $6101
;
;ADR1_LOW  = $6102
;ADR1_HIGH = $6103
;
;RES_LOW   = $6200
;RES_HIGH  = $6201 
;
;SUB16: SEC      ; CLEAR CARRY BIT
;       CLD      ; CLEAR DECIMAL BIT
;
;       LDA ADR1_LOW
;       SBC ADR2_LOW
;       STA RES_LOW
;
;       LDA ADR1_HIGH
;       SBC ADR2_HIGH
;       STA RES_HIGH
;
;       RTS

PTR1_LOW   = $00 ; this is where we store our address
PTR1_HIGH  = $01 ; because it is in the "zero page" it is easier to increment

INIT1_LOW  = $13 ; memory adress to start loop, we will be filling in memory "backwards" 
INIT1_HIGH = $61 ; need to move this further to make room for data... but not there yet anyway    

; same thing, but one address behind

PTR2_LOW   = $02 
PTR2_HIGH  = $03 

INIT2_LOW  = $11 
INIT2_HIGH = $61 

INCREMENT = $02 ; how far back to move each iteration
ITER      = 10  ; how many iterations

      ; set the top of our range as INIT1_LOW,INIT1_HIGH stored at PTR1_LOW,PTR1_HIGH
      LDA #INIT1_LOW
      STA PTR1_LOW
      LDA #INIT1_HIGH
      STA PTR1_HIGH

      ; same thing for the address behind
      LDA #INIT2_LOW
      STA PTR2_LOW
      LDA #INIT2_HIGH
      STA PTR2_HIGH

      LDX #ITER  ; Loop 20 times
      LDY #0

; At this point, I am looping through the addresses I want
; Currently I am just taking the difference of the low bit
; assuming the diff is low, in the high bit I store a zero
; what I need to add next is a branch on plus that increments a result address

LOOP: SEC
      CLD
      LDA (PTR1_LOW),Y
      SBC (PTR2_LOW),Y

      STA (PTR1_LOW),Y

      LDA #0
      LDY #1
      STA (PTR1_LOW),Y
      LDY #0

      ;LDA #$EE        
      ;STA (PTR1_LOW),Y    ; store A to the address held in PTR1_LOW,PTR1_HIGH
      ;LDA #$DD
      ;STA (PTR2_LOW),Y    ; store A to the address held in PTR2_LOW,PTR2_HIGH

      ; deincrement PTR1_LOW,PTR1_HIGH with 16-bit subtraction
      SEC            
      LDA PTR1_LOW
      SBC #INCREMENT
      STA PTR1_LOW
      LDA PTR1_HIGH
      SBC #0         ; assuming here the high bit of int deincrement is zero
      STA PTR1_HIGH

      ; same thing for one address behind
      SEC            
      LDA PTR2_LOW
      SBC #INCREMENT
      STA PTR2_LOW
      LDA PTR2_HIGH
      SBC #0         ; assuming here the high bit of int deincrement is zero
      STA PTR2_HIGH

      DEX            ; loop until X is zero, will need to do this differently for real data with 2000 iterations
      BNE LOOP

ADR2_LOW  = $6100 ; this is a static location, but what I really need is to be able to increment this
ADR2_HIGH = $6101

ADR1_LOW  = $6102
ADR1_HIGH = $6103

RES_LOW   = $6200
RES_HIGH  = $6201 

SUB16: SEC      ; CLEAR CARRY BIT
       CLD      ; CLEAR DECIMAL BIT

       LDA ADR1_LOW
       SBC ADR2_LOW
       STA RES_LOW

       LDA ADR1_HIGH
       SBC ADR2_HIGH
       STA RES_HIGH

       RTS

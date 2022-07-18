;; The synth and song from Fuzzdealer.. brought to win32/Crinkler platform.
;; This version has almost forgotten its past life as a Linux SDL2 binary.
;; And yes - it is just as I thought back then: The same show could be
;; compressed to smaller size with the Win32 Crinkler tools. I knew it.
;; Since then I've learned new tricks on Linux, too, so I'm not yet sure
;; which platform comes with more overhead, Win32&OpenGL or Linux&SDL2&OpenGL.

;; I'm compiling currently like this:
;; nasm -f win32 -o synth2.obj sound.nasm

BITS 32

;; We output a complete RIFF file. Here is the header.
;; It conveniently defines the audio format, sample rate etc.
;; They need to be hardcoded on the C++ -side, though.
global  _RIFF_header
;; FIXME: Nice "SYNTH_CAP_NCHANNELS" etc for convenience.
	AUDIO_DURATION_SAMPLES equ 0x800000    ; Approx 174 seconds at 48kHz
	AUDIO_NUMCHANNELS equ 1
SEGMENT	.rdata
    _RIFF_header:
    ;; /* "RIFF" */
	dd "RIFF"
    ;;  /* Chunk size (whole file sz -8) */ AUDIO_BUF_SIZE_IN_BYTES + 36,
    ;;  /* Maybe we lie.. just a bit.. sndPlaySound() doesn't care: */
	dd AUDIO_DURATION_SAMPLES * AUDIO_NUMCHANNELS * 4
    ;;  /* "WAVE" .. means there will be "fmt " and "data" after. */
	dd "WAVE"
    ;; /* Format subchunk: */
    ;; /* "fmt " and size of it*/ 
	dd "fmt ", 16
    ;; /* Audio format, WAVE_FORMAT_IEEE_FLOAT==3 and #channels */ 
	dw 3, AUDIO_NUMCHANNELS
    ;; /* Sample rate */ MZK_RATE,
sample_rate:
	dd 48000
    ;;/* Byte rate */ MZK_RATE* AUDIO_NUMCHANNELS * sizeof(float),
	dd 48000 * AUDIO_NUMCHANNELS * 4
    ;; /* Bytes-per-block */ (AUDIO_NUMCHANNELS * sizeof(float)) | /* bits-per-sample */ ((8 * sizeof(float)) << 16),
	dw AUDIO_NUMCHANNELS * 4, 32
    ;; /* Data subchunk: */
    ;; /* "data" */ 0x61746164,
	dd "data"
    ;;/* Subchunk size */
	dd AUDIO_DURATION_SAMPLES * AUDIO_NUMCHANNELS * 4

;; Make data global, so crinkler can shift things around.
;;;  ------------------------- synth constants

SEGMENT .rdata
syn_c0freq:
syn_basevol:
   	dd	0.004138524	; 0x3b879c75; close to MIDI note 24 freq / 48k * 2 * pi	
;;; 	dd	0.016554097	; 0x3c879c75; close to MIDI note 0x30
syn_freqr:
;;; 	dd	1.0594630943592953  ; freq. ratio between notes
	dd	1.0594622	; 0x3f879c75; close to 1.0594630943592953
syn_ticklen:
	dd	0x1770   	; sequencer tick length. 0x1770 is 120bpm \w 4-tick note

global syn_c0freq, syn_freqr, syn_ticklen
;; global syn_seq_duration

;; SEGMENT .data
;; syn_seq_duration:
;;	dd 100*48000 ; (in frames/blocks/whatyoucallem)
;; 	dd 0x500000	; close enough.. not really.. but WOW, the interesting part of the song comes after buffer overrun!!
DURATION equ 0x500000

SEGMENT .bss
global syn_currentf, syn_pipeline
;;; State of my monophonic fuzzdelay synth contains frequency and other variables.
syn_currentf:
	resd	1
;;; Synth pipeline variables - used one by one, in order.
;;; Some of these are "patch parameters" that are supposed to be set by the note stream.
;;; Some pipeline variables are only for internal use.
syn_pipeline:
syn_env_state:	resd	1
;;; Integer parameters start from here:
syn_params:
syn_tiks:	resd	1	; (used as parameter #0)
syn_steplen:	resd	1	; (not used as parameter #1) ... etc
syn_nvol:	resd	1
syn_dlen:	resd	1
syn_dvol:	resd	1
syn_lsrc:	resd	1
syn_lvol:	resd	1
syn_p7:		resd	1

SEGMENT .bss
global syn_delay_line_space
;; At the heart of this particular fuzzdelay synth are the delay lines.
;; My hack relies on some silence before actual buffers.

syn_delay_line_space:
syn_padding:
	resd	0x400000
syn_rec:
	resd	0x800000
syn_dly:
	resd	0x800000

global _riff_data
SEGMENT .bss
_riff_data:
	resd     AUDIO_DURATION_SAMPLES * AUDIO_NUMCHANNELS

;; The synth function that will be called from intro init.
SEGMENT .text

global _make_RIFF@0
_make_RIFF@0:

;; The stack will be used very little here.. Should it? The old synth used a global
;; state because it was real-time. Now off-line things could be kept in stack..
;; But zero-init in bss should be very fine, too.. better save space..

	pushad

	;; Copy the RIFF header to beginning of output (will leave 0 in ECX for next steps, btw..):
	mov	edi, _riff_data
	mov	esi, _RIFF_header
	push	11
	pop	ecx
	rep	movsd
	;; After this: edi points to beginning of sound output. ecx is 0.
	;; Then just output sound.

;; Dedicated registers:
;;   ESI == pointer to next sequencer event
;;   EDI == During each loop iteration: pointer to next pipeline variable
;;   EAX == The temp of temps
;;   EBX == (not yet in dedicated use here - could optimize something with it)
;;   ECX == output frame counter (i.e., blocks of samples)
;;   EDX == Preferred temporary data register together with EAX
;;   EBP == Beginning of output buffer

 	mov	esi, dword syn_seq_data
	mov	ebp, edi	; EBP is now the base of output operations

aud_buf_loop:
	mov	edi, syn_pipeline
	;; ---------------------------------------------------------------------
	;; Only reconfigure state when step length has been reached:
	mov	eax, dword [syn_steplen]
	scasd  ; EDI--->syn_tiks  ZF if env_state==steplen
	jne	do_sample
	
	;; ---------------------------------------------------------------------
	;; Read bytes and reconfigure state.
	;; First, reset envelope state:
	xor	eax, eax
	mov	dword [syn_env_state], eax

read_from_sequence:
	;; Read next byte from sequence to DL, and update counter:
	lodsb
	;; Shift last bit out to see if we have a 7-bit note or a control byte:
 	shr	al, 1
	mov	edx, eax	; EAX was 0, so now DL==EDX==EAX==AL. mov changes no flag.
	jnc	new_note	; Break loop when a note event arrives after N parameter sets.

	lodsb
	;; Store parameter
	mov	[syn_params + 4*edx],eax
	jmp	read_from_sequence
	
new_note:
	;; We have a note. Compute new frequency or remain at 0 Hz ("silence")
	;; NOTE: Zero flag must be set if DL==0 !
	fldz
	jz	store_frequency
	fadd	dword [syn_c0freq]
pow_to_frequency:
	fmul	dword [syn_freqr]
	dec	dl
	jnz	pow_to_frequency
store_frequency:
 	fstp	dword[syn_currentf]
	
do_sample:
	;; Re-compute based on params (may have just changed)
	mov	eax, [edi]
	mul	dword [syn_ticklen]
 	scasd	; EDI ---> syn_steplen
 	mov	dword [edi], eax

	fld1							; (1)
 	fild	dword [syn_env_state] ; (1 ienv)
	fidiv	dword [edi]					; (1 rise)
	fsubp   						; (fall)
	
	;; Intend to play sin(2pi*frequency*framecount/srate)

 	fld	dword [syn_currentf]	;(fall note)
   	fimul	dword [syn_env_state]	; (fall note*iphase)
  	fld	st0			; (fall note*iphase note*iphase)
 	fadd	st0			; (fall note*iphase 2*note*iphase)
  	fadd	st0			; (fall note*iphase 4*note*iphase)
	fsin				; (fall note*iphase sin(4*note*iphase))
	faddp	   			; (fall note*iphase+sin(4*note*iphase))
	fsin				; (fall sin(note*iphase+sin(4*note*iphase)))
	fmulp   			; (fall*sin(note*iphase+sin(4*note*iphase)))
					; == (audio)

	;; Overall volume:
	scasd	; EDI ---> syn_nvol
	fimul	dword [edi]
	fmul	dword [syn_basevol]

	;; ---- Delay thingy.
	;; collect shorter delay:
	;; Compute short delay length:
	scasd	; EDI ---> syn_dlen
	mov	eax, [edi]
	mul	dword [syn_ticklen]
	mov	edx, ecx
	sub	edx, eax
	
	fld	dword [syn_dly + 4*edx] ;(audio delayed)
	scasd	; EDI ---> syn_dvol
	fimul	dword [edi]
	fmul	dword [syn_basevol]
	faddp				;(audio+dvol*delayed)
	fld	st0			;(audio+dvol*delayed audio+dvol*delayed)
	
	;; collect longer delay from history, with loop volume:
	scasd	; EDI ---> syn_lsrc
	mov	eax, [edi]
	mul	dword [syn_ticklen]
	mov	edx, ecx
	sub	edx, eax
	
	fld	dword [syn_rec + 4 * edx] ;(dly dly looped)
	scasd	; EDI ---> syn_lvol
	fimul	dword [edi]
	fmul	dword [syn_basevol] ;(dly dly lvol*looped)
	faddp			;(dly dly+lvol*looped)
	fld	st0		;(dly mix mix)
nomore:	
	
	;; ---- Delay thingy ends.

;;; Make a smooth distortion for output (some +8 bytes compressed..)
	fld	st0		; (x x)
	fabs			; (x |x|)
	fld1			; (x |x| 1)
	faddp			; (x 1+|x|)
	fdivp			; (x/(1+|x|))

	;;  Finally store. Fp stack is now: (dly mix final)
	fstp	dword [ebp + 4*ecx]		; -> buffer (dly mix)
	fstp	dword [syn_rec + 4*ecx] 	; -> mix (dly)
	fstp	dword [syn_dly + 4*ecx] 	; -> delay ()
	
	inc	ecx
	inc	dword [syn_env_state]
	
	;;cmp     dword [syn_seq_duration], ecx ; length in bytes
	cmp	ecx, DURATION
	
	jnz	aud_buf_loop
	
    popad
	ret


;; ------------ The song data here:

%define STEP_LEN(val)  (0<<(1))+1,(val) ,
%define NOTE_VOL(val)  (2<<(1))+1,(val) ,
%define DLAY_LEN(val)  (3<<(1))+1,(val) ,
%define DLAY_VOL(val)  (4<<(1))+1,(val) ,
%define LOOP_SRC(val)  (5<<(1))+1,(val) ,
%define LOOP_VOL(val)  (6<<(1))+1,(val) ,

;;; Let me make these notes as octaves and note names...
%define c 0
%define C 1
%define d 2
%define D 3
%define e 4
%define f 5
%define F 6
%define g 7
%define G 8
%define a 9
%define b 11
%define n(p,o) ((-24 + p+12*o)<<1),
%define pause  0,

SEGMENT .rdata
global syn_seq_data

syn_seq_data:
	;; Played one byte at a time:
	
	db	STEP_LEN(2) 	; This must be set. Others are nicely 0-inited.
	db	DLAY_VOL(0x75)
	db	NOTE_VOL(0x75)  
	db      DLAY_LEN(3) 
	db	LOOP_VOL(0xff)
	db	LOOP_SRC(16)
	
	;; Intro
 	db	n(f,3) 	n(f,3) 	n(e,4)	n(f,4)
 	db	pause 	n(f,3) 	n(e,4)	n(f,4)
	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause

	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause
	
	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause

	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause
	db	pause 	pause   pause   pause
	
	;; Little variance from this point onwards; start longer steps
	db	pause   n(b,4)	pause   pause
	db	STEP_LEN(8)
	db	pause
	db	pause
	db	pause
	
	db	pause
	db	pause
	db	pause
	db	pause
	

	;; Bass finally in.
	db	n(f,2) pause pause pause
	db	pause  pause pause pause
	db	pause  pause pause pause
	db	pause  pause pause pause
	
	db	pause  pause pause pause
	db	pause  pause pause pause
	db	pause  pause pause pause
	db	pause  pause pause pause
	
	db	pause  pause pause pause
  	db	pause  pause pause pause
	
	;; Then the groovy theme with a "chord"
	
	db	pause  n(f,3) pause n(a,4)
	db	pause  n(b,4) pause n(c,5)
	
	db	pause  n(f,3) pause n(a,4)
	db	pause  n(b,4) pause n(c,5)
	
	db	pause  n(f,3) pause n(a,4)
	db	pause  n(b,4) pause n(c,5)
	
	db	pause  n(f,3) pause n(a,4)

	;; Finale with repeated note
	db	LOOP_SRC(8)
	db 	pause pause pause pause
	
	;; Fade-out with a short loop at low volume
	db	pause LOOP_SRC(4) LOOP_VOL(0x30)

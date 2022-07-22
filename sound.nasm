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
;; FIXME: Nice "SYNTH_CAP_NCHANNELS" etc for convenience.
	AUDIO_DURATION_SAMPLES equ 0x800000    ; Approx 174 seconds at 48kHz
	AUDIO_NUMCHANNELS equ 1
	AUDIO_SAMPLERATE equ 48000
	WAVE_FORMAT_IEEE_FLOAT equ 3 ; From the API
	SIZEOF_IEEE_FLOAT equ 4

SEGMENT	.rdata
global  _RIFF_header
    _RIFF_header:
    ;; /* "RIFF" */
	dd "RIFF"
    ;;  /* Chunk size (whole file sz -8) */ AUDIO_BUF_SIZE_IN_BYTES + 36,
    ;;  /* Maybe we lie.. just a bit.. sndPlaySound() doesn't care: */
	dd AUDIO_DURATION_SAMPLES * AUDIO_NUMCHANNELS * SIZEOF_IEEE_FLOAT
    ;;  /* "WAVE" .. means there will be "fmt " and "data" after. */
	dd "WAVE"
    ;; /* Format subchunk: */
    ;; /* "fmt " and size of it*/ 
	dd "fmt ", 16
    ;; /* Audio format, WAVE_FORMAT_IEEE_FLOAT==3 and #channels */ 
	dw WAVE_FORMAT_IEEE_FLOAT, AUDIO_NUMCHANNELS
    ;; /* Sample rate */ MZK_RATE,
sample_rate:
	dd AUDIO_SAMPLERATE
    ;;/* Byte rate */
	dd AUDIO_SAMPLERATE * AUDIO_NUMCHANNELS * SIZEOF_IEEE_FLOAT
    ;; /* Bytes-per-block */ (AUDIO_NUMCHANNELS * sizeof(float)) | /* bits-per-sample */ ((8 * sizeof(float)) << 16),
	dw AUDIO_NUMCHANNELS * SIZEOF_IEEE_FLOAT, 8 * SIZEOF_IEEE_FLOAT
    ;; /* Data subchunk: */
    ;; /* "data" */ 0x61746164,
	dd "data"
    ;;/* Subchunk size */
	dd AUDIO_DURATION_SAMPLES * AUDIO_NUMCHANNELS * SIZEOF_IEEE_FLOAT

;; Make data global, so crinkler can shift things around.
;;;  ------------------------- synth constants
;;; (moved to per-song part, actually..)

;; global syn_seq_duration

;; SEGMENT .data
;; syn_seq_duration:
;;	dd 100*48000 ; (in frames/blocks/whatyoucallem)
;; 	dd 0x500000	; close enough.. not really.. but WOW, the interesting part of the song comes after buffer overrun!!
DURATION equ 0x500000

SEGMENT .bss
global syn_currentf, syn_pipeline
;;; State of my monophonic fuzzdelay synth contains frequency and other variables.
syn_currentf:	resd	1
;;; Synth pipeline variables - used one by one, in order.
;;; Some of these are "patch parameters" that are supposed to be set by the note stream.
;;; Some pipeline variables are only for internal use.
syn_pipeline:
syn_BASE:
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
;;   EDI == Beginning of output buffer
;;   EAX == The temp of temps
;;   EBX == Base pointer to synth / song constants
;;   ECX == output frame counter (i.e., blocks of samples)
;;   EDX == Preferred temporary data register together with EAX
;;   EBP == Base pointer to the synth state variable package.
;;   ESP == Stack top would be usable for intermediates/calls

	mov	ebp, syn_BASE
	mov	ebx, synconst_BASE
;; 	mov	esi, syn_seq_data
	mov	esi, ebx

;; Maybe this is a space-saver, even with crinkler.. 
%define ADDR(r,base,a)   r + ((a) - base)
%define SPAR(par) ADDR(ebp, syn_BASE, par)
%define SCONST(cnst) ADDR(ebx, synconst_BASE, cnst)

aud_buf_loop:
	;; ---------------------------------------------------------------------
	;; Only reconfigure state when step length has been reached:
	mov	eax, dword [SPAR(syn_steplen)]
	sub	eax, dword [SPAR(syn_env_state)]
	;; Observe situation after this:
	;; If ZF (due to subtraction) then:
	;;   - step is at end (state==len), so we read data;
	;;   - this occurs on first entry because steplen==state==0
	;;   - incidentally, or less incidentally, EAX==0
	;; Else:
	;;   - we jump to render sound.
	jne	do_sample
	
	;; ---------------------------------------------------------------------
	;; Read bytes and reconfigure state.
	;; First, reset envelope state:
	;; xor	eax, eax
	mov	dword [SPAR(syn_env_state)], eax

read_from_sequence:
	;; Read next byte from sequence to DL, and update counter:
	lodsb
	;; Shift last bit out to see if we have a 7-bit note or a control byte:
 	shr	al, 1
	mov	edx, eax	; EAX was 0, so now DL==EDX==EAX==AL. mov changes no flag.
	jnc	new_note	; Break loop when a note event arrives after 0-N parameter sets.

	lodsb
	;; Store parameter
	mov	[SPAR(syn_params) + 4*edx],eax
	jmp	read_from_sequence
	
new_note:
	;; We have a note. Compute new frequency or remain at 0 Hz ("silence")
	;; NOTE: Zero flag must be set if DL==0 !
	fldz
	jz	store_frequency
	fadd	dword [SCONST(synconst_c0freq)]
pow_to_frequency:
	fmul	dword [SCONST(synconst_freqr)]
	dec	dl
	jnz	pow_to_frequency
store_frequency:
 	fstp	dword[SPAR(syn_currentf)]

do_sample:
	;; Re-compute based on params (may have just changed)
	mov	eax, [SPAR(syn_tiks)]	; was EDI ---> syn_tiks
	mul	dword [SCONST(synconst_ticklen)]	; global tick length
 	mov	dword [SPAR(syn_steplen)], eax	; store step length

envelope:
	fild	dword [SPAR(syn_steplen)]	; ( steplen )
	fisub	dword [SPAR(syn_env_state)]	; ( steplen - state)
	fidiv	dword [SPAR(syn_steplen)]	; ( [len-state]/len =: fall )

	;; Intend to play sin(2pi*frequency*framecount/srate)
phasemod:
 	fld	dword [SPAR(syn_currentf)]	; (fall note)
   	fimul	dword [SPAR(syn_env_state)]	; (fall note*iphase)
  	fld	st0			; (fall note*iphase note*iphase)
 	fadd	st0			; (fall note*iphase 2*note*iphase)
	fadd	st0			; (fall note*iphase 4*note*iphase)
	fsin				; (fall note*iphase sin(4*note*iphase))
	fmul	st2			; (fall note*iphase fall*phaseplus)
	fmul	st2			; (fall note*iphase fall*phaseplus)
	faddp	   			; (fall note*iphase+sin(4*note*iphase))
	fsin				; (fall sin(note*iphase+sin(4*note*iphase)))
	fmulp   			; (fall*sin(note*iphase+sin(4*note*iphase)))
					; == (audio)

	;; Overall volume:
	fimul	dword [SPAR(syn_nvol)]
	fmul	dword [SCONST(synconst_basevol)]	; FIXME: Constants same trick with base!?

delays:
	;; ---- Delay thingy.
	;; collect shorter delay:
	;; Compute short delay length:
	mov	eax, [SPAR(syn_dlen)]
	mul	dword [SCONST(synconst_ticklen)]
	mov	edx, ecx
	sub	edx, eax
	
	fld	dword [syn_dly + 4*edx] ;(audio delayed)
	fimul	dword [SPAR(syn_dvol)]
	fmul	dword [SCONST(synconst_basevol)]
	faddp				;(audio+dvol*delayed)
	fld	st0			;(audio+dvol*delayed audio+dvol*delayed)
	
	;; collect longer delay from history, with loop volume:
	;; ehm.. doesn't have to be longer, really.. interesting, unplanned facet..
	;; Playing with two quite similar delay lines here.
	mov	eax, [SPAR(syn_lsrc)]
	mul	dword [SCONST(synconst_ticklen)]
	mov	edx, ecx
	sub	edx, eax
	
	fld	dword [syn_rec + 4 * edx] ;(dly dly looped)
	fimul	dword [SPAR(syn_lvol)]
	fmul	dword [SCONST(synconst_basevol)] ;(dly dly lvol*looped)
	faddp			;(dly dly+lvol*looped)
	fld	st0		;(dly mix mix)
	
	;; ---- Delay thingy ends.

distortion:
;;; Make a smooth distortion for output (some +8 bytes compressed..)
;;; Distortion affects final output, not delay lines. Hmm... could re-design a bit?
	fld	st0		; (x x)
	fabs			; (x |x|)
	fld1			; (x |x| 1)
	faddp			; (x 1+|x|)
	fdivp			; (x/(1+|x|))

outputs:
	;;  Finally store. Fp stack is now: (dly mix final)
	fstp	dword [edi + 4*ecx]		; -> buffer (dly mix)
	fstp	dword [syn_rec + 4*ecx] 	; -> mix (dly)
	fstp	dword [syn_dly + 4*ecx] 	; -> delay ()

book_keeping:

	inc	dword [SPAR(syn_env_state)]
	
	inc	ecx
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
%define A 10
%define b 11
%define n(p,o) ((-24 + p+12*o)<<1),
%define pause  0,


SEGMENT .rdata
global synconst_START
;; Hmm... This is basically about "tuning and fine-tuning"
;; So make it more like "per-song" data than "pre-set"
synconst_START:
synconst_c0freq:
   	dd	0.004138524	; 0x3b879c75; close to MIDI note 24 freq / 48k * 2 * pi	
;;; 	dd	0.016554097	; 0x3c879c75; close to MIDI note 0x30
synconst_basevol:
;;;	dd	0.004138524	; (Fuzzdealer) 0x3b879c75; close to MIDI note 24 freq / 48k * 2 * pi	
	dd	0.0078125	; 1/128.0 hexrepr 0x3c000000. Makes setvol(0x80) equal 1.0. Easy.
synconst_freqr:
;;; 	dd	1.0594630943592953  ; freq. ratio between notes
	dd	1.0594622	; 0x3f879c75; close to 1.0594630943592953
synconst_ticklen:
;;	dd	0x1770   	; sequencer tick length. 0x1770 is 120bpm \w 4-tick note
	dd	0x1200   	; sequencer tick length.
synconst_BASE:

syn_seq_data:
;; The only must-set is STEP_LEN(len).
;; Otherwise the step sequencer will not start stepping.
;; Everything else can be left in their BSS zero-init.
;; But the default volume is 0, so NOTE_VOL(vol) is another one.


;; A third go at this synth...
	db	NOTE_VOL(0x30)
	db	DLAY_VOL(0x40)
	db      DLAY_LEN(2) 
	db	LOOP_VOL(0x80)
;;	db	LOOP_SRC(16)
	
	;; Intro
	db	LOOP_SRC(12)

	db	STEP_LEN(8)   n(d,2) n(d,2) 
	db	LOOP_SRC(16)

	db	pause pause pause pause
	db	LOOP_SRC(32)
	db	DLAY_LEN(3) DLAY_VOL(0x60) NOTE_VOL(0x20)
	db	n(a,5) n(g,5) pause pause
	db	LOOP_SRC(64)
	db	n(g,5) n(f,5) pause pause
	db	LOOP_SRC(96)
	db	n(e,5) pause pause pause
	db	LOOP_SRC(128)
	db	pause pause pause pause
	db	pause pause pause pause
	db	pause pause pause pause
	db	pause pause pause pause
	db	pause pause pause pause

	db	STEP_LEN(128)
	db	pause pause pause pause



%if 0
;; Just something to try while picking this up after half a decade
	db	STEP_LEN(1) 	; This must be set. Others are nicely 0-inited.
	db	DLAY_VOL(0x75)
	db	NOTE_VOL(0x75)  
	db      DLAY_LEN(2) 
	db	LOOP_VOL(0xff)
	db	LOOP_SRC(16)
	
	;; Intro
 	db	n(e,2) 	pause 	n(e,2)	pause
 	db	n(e,2) 	n(e,2)	pause	n(e,2)
 	db	n(e,3) 	pause 	n(e,2)	pause
 	db	n(e,2) 	n(e,2)	pause	n(e,2)

	db      DLAY_LEN(6) 
	db	DLAY_VOL(0x35)
	
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause

	db	n(e,3)	pause	pause	pause
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause

	db	pause	pause	pause	pause
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause

	db	STEP_LEN(2)
	db	n(e,9)		pause	
	db	pause		pause	
	db	pause		n(b,9)	
	db	pause		n(d,10)	
	db	LOOP_SRC(32)


	db	n(e,9)		n(b,9)
	db	pause		pause	
	db	pause		pause	
	db	pause		pause	
	db	LOOP_SRC(64)

	db	pause		pause	
	db	pause		pause	
	db	pause		pause	
	db	pause		pause	

	db	pause		pause	
	db	pause		pause	
	db	pause		pause	
	db	pause		pause	

	db	pause		pause	
	db	pause		pause	
	db	pause		pause	
	db	pause		pause	

	db	pause		pause	
	db	pause		pause	
	db	pause		pause	
	db	pause		pause	

	db	pause		pause	
	db	pause		pause	
	db	pause		pause	
	db	pause		pause	

	db	pause		pause	
	db	pause		pause	
	db	pause		pause	
	db	pause		pause	

	db	STEP_LEN(8)
	db	DLAY_LEN(2)	DLAY_VOL(0xd0)

	db	n(a,4)	pause	pause	pause
	db	LOOP_SRC(64)
	db	n(g,4)	pause	pause	pause
	db	LOOP_SRC(96)
	db	n(f,4)	pause	pause	pause
	db	LOOP_SRC(128)
	db	pause	pause	pause	pause
	db	n(e,3)	pause	pause	pause
	db	pause	pause	pause	pause

	db	pause	pause	pause	pause
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause
	db	pause	pause	pause	pause
%endif

%if 0
;; The one from Fuzzdealer (2016).
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
%endif

;; Clean start for very minimalistic sound.

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

;; Some kind of own "song sequence data" would be optimal here
;; (no need to re-set ESI after copying header to output. Just read it from here.) 

syn_sequence:
	db 0

;; SEGMENT .data
;; syn_seq_duration:
;;	dd 100*48000 ; (in frames/blocks/whatyoucallem)
;; 	dd 0x500000	; 0x500000/48kHz ~= 109 seconds.
DURATION equ 0x500000

SEGMENT .bss
global syn_BASE
;;; State of my monophonic fuzzdelay synth contains frequency and other variables.
syn_BASE:
syn_currentf:	resd	1
;;; Synth pipeline variables - (from old fuzzdelay synth it seems..)
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

	pushad

copy_riff_header:
	;; Copy the RIFF header to beginning of output (will leave 0 in ECX for next steps, btw..):
	mov	edi, _riff_data
	mov	esi, _RIFF_header
;;	push	11
;;	pop	ecx
	mov	ecx, 44
	rep	movsd
	;; After this: edi points to beginning of sound output. ecx is 0.
	;; Then just output sound.

prepare_for_loop:
;; Dedicated registers:
;;   ESI == (could be?: pointer to next sequencer event)
;;   EDI == Beginning of output buffer
;;   EAX == The temp of temps
;;   EBX == Base pointer to synth / song constants
;;   ECX == output frame counter (i.e., blocks of samples)
;;   EDX == Preferred temporary data register together with EAX
;;   EBP == Base pointer to the synth state variable package.
;;   ESP == Stack top would be usable for intermediates/calls

	mov	ebp, syn_BASE
	mov	ebx, synconst_BASE

;; Maybe this is a space-saver, even with crinkler.. 
%define ADDR(r,base,a)   r + ((a) - base)
%define SPAR(par) ADDR(ebp, syn_BASE, par)
%define SCONST(cnst) ADDR(ebx, synconst_BASE, cnst)
;; %define SCONST(cnst) cnst

aud_buf_loop:
	;; ---------------------------------------------------------------------
	;; Only reconfigure state when step length has been reached.
	;; Constant tick length for this song.. how to handle first tick now?
	mov	eax, dword [SCONST(synconst_ticklen)]
	sub	eax, dword [SPAR(syn_env_state)]
	;; Observe situation after this, after normal tick:
	;; If ZF (due to subtraction) then:
	;;   - step is at end (state==len), so we read data;
	;;   - (NOT HERE: this occurs on first entry because steplen==state==0)
	;;   - incidentally, or less incidentally, EAX==0
	;; Else:
	;;   - we jump to render sound.
	jne	do_sample
	
	;; ---------------------------------------------------------------------
	;; Read bytes and reconfigure state.
	;; First, reset envelope state (EAX==0, as we just observed):
	mov	dword [SPAR(syn_env_state)], eax

read_from_sequence:
	;; Read next byte from sequence to DL, and update counter:
	lodsb
	;; Dig a note somehow..
	;;mov	al, 20 ; Meanwhile: Fake it
	
new_note:
	;; We have a note. Compute new frequency or remain at 0 Hz ("silence")
	fldz
	cmp	al, 0
	jz	store_frequency
	fadd	dword [SCONST(synconst_c0freq)]
pow_to_frequency:
	fmul	dword [SCONST(synconst_freqr)]
	dec	ax
	jnz	pow_to_frequency
store_frequency:
 	fstp	dword[SPAR(syn_currentf)]

do_sample:
sine:
 	fld	dword [SPAR(syn_currentf)]	; (fall note)
   	fimul	dword [SPAR(syn_env_state)]	; (fall note*iphase)
	fsin
%if 0
sawwave:
 	fld	dword [SPAR(syn_currentf)]	; (fall note)
   	fimul	dword [SPAR(syn_env_state)]	; (fall note*iphase)
	fld	st0			; (fall note*iphase note*iphase)
  	frndint				; (fall note*iphase round(note*iphase))
 	fsubp				; (fall sawwave)
%endif

delayed:
	mov	eax, ecx
	sub	eax, [SCONST(synconst_ticklen)]
	sub	eax, [SCONST(synconst_ticklen)]
	sub	eax, [SCONST(synconst_ticklen)]
	fld	dword[edi + 4*eax]
	fmul	dword [SCONST(synconst_delayvol)]
	faddp

outputs:
	fstp	dword [edi + 4*ecx]		; -> buffer
;;	fstp	dword [syn_dly + 4*ecx] 	; -> delay line

book_keeping:
	inc	dword [SPAR(syn_env_state)]
	inc	ecx
	cmp	ecx, DURATION
	
	jnz	aud_buf_loop

returningments:
	popad
	ret


SEGMENT .rdata
global synconst_START
;; Hmm... This is basically about "tuning and fine-tuning"
;; So make it more like "per-song" data than "pre-set"
synconst_START:
synconst_BASE:
synconst_c0freq:
;;;  	dd	0.004138524	; 0x3b879c75; close to MIDI note 24 freq / 48k * 2 * pi	

	dd	0.0006813125
;;;	dd	0.000682830810547
;;; 	dd	0.016554097	; 0x3c879c75; close to MIDI note 0x30
synconst_ticklen:
;;	dd	0x1770   	; sequencer tick length. 0x1770 is 120bpm \w 4-tick note
;;	dd	0x1200   	; sequencer tick length.
	dd	0x1200   	; sequencer tick length.
synconst_freqr:
;;; 	dd	1.0594630943592953  ; freq. ratio between notes
	dd	1.0594622	; 0x3f879c75; close to 1.0594630943592953
synconst_delayvol:
	dd	0.5

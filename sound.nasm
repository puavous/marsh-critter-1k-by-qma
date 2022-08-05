;; Clean start for very minimalistic sound.
;; This version tries to re-purpose the minified shader string
;; as a monophonic synth sequence.. converting
;; "vec3(" and "exp(" into licks in a song... creating a strange symbiosis
;; between graphics and sound through source code and an obfuscator in
;; between. Who composed the song? Me, creator of the shader minifier?
;; Software? All of us together?

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
;; Could have a label for the integer, if some computations need it:
;;sample_rate:
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

%if 0
syn_sequence:
	;; Try something from just above, repeated:
	db "fmt     fmt     fmt datadatadata"
	db "fmt fmt fmt fmt fmt datadatadata"
	;; Yep.. let us 'compose a song' called Etudes of Minified Shader Code:
	db "float f=length(y-vec4(0,0,0,2).xyz)-vec4(0,0,0,2).w;"
	db "float f=length(y-vec4(0,0,0,2).xyz)-vec4(0,0,0,2).w;"
%endif
%if 0
	db "#version 140\n"
	db  "uniform ivec4 u;"
	db  "float v=u.x/1e3;"
	db "float n(vec3 y)"
	db "{"
	db   "float f=length(y-vec4(0,0,0,2).xyz)-vec4(0,0,0,2).w;"
	db   "for(int r=0;r<6;r++)"
	db     "f=-log(exp(-4*f)+exp(-4*(length(y-vec4(v/3*sin(r+v),sin(v+v*r),v/3*cos(r+v),1).xyz)-vec4(v/3*sin(r+v),sin(v+v*r),v/3*cos(r+v),1).w)))/4;"
	db   "f=-log(exp(-6*f)+exp(-6*(y.y-(0-v/10))))/6;"
	db   "return f;"
	db "}"
%endif

%if 0
   	db "f=-log(exp(-6*f)+exp(-6*(y.y-(0-v/10))))/6;"
   	db "f=-log(exp(-6*f)+exp(-6*(y.y-(0-v/10))))/6;"
   	db "return f;"
   	db "return f;"
   	db "return f;"
   	db "return f;"
     	db "f=-log(exp(-4*f)+exp(-4*(length(y-vec4(v/3*sin(r+v),sin(v+v*r),v/3*cos(r+v),1).xyz)-vec4(v/3*sin(r+v),sin(v+v*r),v/3*cos(r+v),1).w)))/4;"
   	db "f=-log(exp(-6*f)+exp(-6*(y.y-(0-v/10))))/6;"
;;	db "vec4(vec4(vec4(vec4(vec4(vec4(vec4(vec4(vec4(vec4(vec4("
;;	db "float float float float float float float float float  "
;;	db "                           ", 0,0,0,0,0,0,0,0
%endif

;; SEGMENT .data
;; syn_seq_duration:
;;	dd 100*48000 ; (in frames/blocks/whatyoucallem)
;; 	dd 0x500000	; 0x500000/48kHz ~= 109 seconds.
DURATION equ 0x500000

SEGMENT .bss
global syn_BASE
;;; Zero-initialized state of the synth.
syn_BASE:
syn_currentf:	resd	1
syn_env_state:	resd	1


%if 0
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
%endif

global _riff_data
SEGMENT .bss
syn_padding:
	resd	0x400000
_riff_data:
	resd     AUDIO_DURATION_SAMPLES * AUDIO_NUMCHANNELS


;; ---------------------------------------------------------------
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

	;; Let us try complete re-purposing.. Point to shader string:
extern ?shader_frag@@3PBDB
	mov	esi, [?shader_frag@@3PBDB]

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

	;; Hack silence after end of show:
	cmp	ecx, 48000*20
	jl	noupd
	xor	al,al
noupd:
	;; Dig a note somehow.. and have ZF indicate pause somehow.
;;	cmp	al, 0
	;; Idea: Rotate this mask pattern every now and then? Maybe based
	;; on some not-too-common input character?
	;; Just re-set pattern from input sometimes?
	;; It will take the tonal atmosphere to another place.
	 and	al, 00101101b
	;; and	al, 10010110b
	;;and	al, 01001011b
	;;and	al, 00001011b ; or other modifications..

	
new_note:
	;; We have a note. Compute new frequency or remain at 0 Hz ("silence")
	fldz
	jz	store_frequency
	fadd	dword [SCONST(synconst_c0freq)]
.pow_to_frequency:
	fmul	dword [SCONST(synconst_freqr)]
	dec	ax
	jnz	.pow_to_frequency
store_frequency:
 	fstp	dword[SPAR(syn_currentf)]

do_sample:
 	fld	dword [SPAR(syn_currentf)]	; (notefreq)
   	fimul	dword [SPAR(syn_env_state)]	; (notefreq*iphase)
	;; Sine wave - tiniest signal I can think of in x87..
	fsin

	;; Amplitude envelope costs some 9 bytes:
	fild	dword [SCONST(synconst_ticklen)]
	fisub	dword [SPAR(syn_env_state)]
	fidiv	dword [SCONST(synconst_ticklen)]
	fmulp
%if 0
	;; Saw wave.. aliasing a lot for good and bad..
	fld	st0			; (fall note*iphase note*iphase)
  	frndint				; (fall note*iphase round(note*iphase))
 	fsubp				; (fall sawwave)
%endif

delayed:
	mov	eax, ecx
	sub	eax, [SCONST(synconst_delaylen)]
	fld	dword[edi + 4*eax]
	fmul	dword [SCONST(synconst_delayvol)]
	faddp

outputs:
	fstp	dword [edi + 4*ecx]		; -> buffer

book_keeping:
	inc	dword [SPAR(syn_env_state)]
	inc	ecx
;;	cmp	ecx, DURATION
	cmp	ecx, [SCONST(synconst_duration)]
	
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
;;  	dd	0.004138524	; 0x3b879c75; close to MIDI note 24 freq / 48k * 2 * pi	
;;	dd	0.0041385279037 ; 0x3b879c7d
	dd	0.0078125	; 0x3c000000
;;;	dd	0.0006813125
;;;	dd	0.000682830810547
;;; 	dd	0.016554097	; 0x3c879c75; close to MIDI note 0x30
synconst_freqr:
;; 	dd	1.0594630943592953  ; 0x3f879c7d freq. ratio between notes
	dd	1.05945575237	; 0x3f879c3f Out of tune..
;;	dd	1.0594622	; 0x3f879c75; close to 1.0594630943592953
synconst_ticklen:
	TICKLEN equ 0x2000
;;	dd	0x1770   	; sequencer tick length. 0x1770 is 120bpm \w 4-tick note
;;	dd	0x1200   	; sequencer tick length.
	dd	1 * TICKLEN
synconst_delaylen:
	dd	6 * TICKLEN	; delay length
synconst_delayvol:
	;;dd	0.5		; 0x3f000000
	dd	0.25
synconst_duration:
	dd	0x003f0000	; 0x3f0000 samples @48kHz is about 86 seconds
	;; I need to cut it at some point - both video and audio..
;;	dd	0x0003f000	; 0x03f000 samples @48kHz is a short verse

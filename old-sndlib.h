#ifndef OLD_SNDLIB_H
#define OLD_SNDLIB_H

/* these are for backwards compatibility -- names changed 10-Apr-00 to use "mus" prefix */
#define clm_printf(str) mus_printf(str)
#define set_clm_datum_type(tfd,type) mus_set_datum_type(tfd,type)
#define open_clm_file_descriptors(tfd,df,ds,dl) mus_file_open_descriptors(tfd,df,ds,dl);
#define close_clm_file_descriptors(tfd) mus_file_close_descriptors(tfd)
#define cleanup_clm_file_descriptors() mus_file_cleanup_descriptors()
#define clm_open_read(arg) mus_file_open_read(arg)
#define clm_open_write(arg) mus_file_open_write(arg)
#define clm_create(arg) mus_file_create(arg)
#define clm_reopen_write(arg) mus_file_reopen_write(arg)
#define clm_close(fd) mus_file_close(fd)
#define clm_seek(tfd,offset,origin) mus_file_seek(tfd,offset,origin)
#define clm_read(fd,beg,end,chans,bufs) mus_file_read(fd,beg,end,chans,bufs)
#define clm_read_chans(fd,beg,end,chans,bufs,cm) mus_file_read_chans(fd,beg,end,chans,bufs,cm)
#define clm_write_zeros(fd,num) mus_file_write_zeros(fd,num)
#define clm_write(tfd,beg,end,chans,bufs) mus_file_write(tfd,beg,end,chans,bufs)
#define float_sound(cbuf,samps,form,buf) mus_float_sound(cbuf,samps,form,buf)
#define create_descriptors()
#define clm_read_any(tfd,beg,chans,nints,bufs,cm) mus_file_read_any(tfd,beg,chans,nints,bufs,cm)
#define unshort_sound(ibuf,samps,form,obuf) mus_unshort_sound(ibuf,samps,form,obuf)
#define complete_filename(tok) mus_file_full_name(tok)

#define c_snd_header_data_size() mus_header_samples()
#define mus_header_data_size() mus_header_samples()
#define c_snd_header_data_location() mus_header_data_location()
#define c_snd_header_chans() mus_header_chans()
#define c_snd_header_srate() mus_header_srate()
#define c_snd_header_type() mus_header_type()
#define c_snd_header_format() mus_header_format()
#define c_snd_header_distributed() mus_header_distributed()
#define c_snd_header_comment_start() mus_header_comment_start()
#define c_snd_header_comment_end() mus_header_comment_end()
#define c_snd_header_type_specifier() mus_header_type_specifier()
#define c_snd_header_bits_per_sample() mus_header_bits_per_sample()
#define c_snd_header_fact_samples() mus_header_fact_samples()
#define c_snd_header_block_align() mus_header_block_align()
#define c_snd_header_loop_mode(w) mus_header_loop_mode(w)
#define c_snd_header_loop_start(w) mus_header_loop_start(w)
#define c_snd_header_loop_end(w) mus_header_loop_end(w)
#define c_snd_header_mark_position(w) mus_header_mark_position(w)
#define c_snd_header_base_note() mus_header_base_note()
#define c_snd_header_base_detune() mus_header_base_detune()
#define c_snd_header_original_format() mus_header_original_format()
#define c_snd_header_datum_size() mus_header_data_format_to_bytes_per_sample()
#define c_snd_header_aux_comment_start(w) mus_header_aux_comment_start(w)
#define c_snd_header_aux_comment_end(w) mus_header_aux_comment_end(w)
#define c_true_file_length() mus_header_true_length()

#define c_snd_datum_size(f) mus_data_format_to_bytes_per_sample(f)
#define mus_datum_size(f) mus_data_format_to_bytes_per_sample(f)
#define c_snd_bytes(f,s) mus_samples_to_bytes(f,s)
#define mus_bytes(f,s) mus_samples_to_bytes(f,s)
#define c_snd_samples(f,s) mus_bytes_to_samples(f,s)
#define mus_samples(f,s) mus_bytes_to_samples(f,s)

#define c_read_header_with_fd(chan) mus_header_read_with_fd(chan)
#define c_read_header(arg) mus_header_read(arg)
#define c_write_header(name,type,srate,chans,loc,size,format,comment,len) mus_header_write(name,type,srate,chans,loc,size,format,comment,len)
#define c_write_header_with_fd(chan,type,srate,chans,loc,size,format,comment,len) mus_header_write_with_fd(chan,type,srate,chans,loc,size,format,comment,len)
#define c_update_header_with_fd(chan,type,siz) mus_header_update_with_fd(chan,type,siz)
#define c_update_header(name,type,siz,srate,format,chans,loc) mus_header_update(name,type,siz,srate,format,chans,loc)
#define c_update_header_comment(name,loc,comment,len,typ) mus_header_update_comment(name,loc,comment,len,typ)
#define bytes_per_sample(frm) mus_data_format_to_bytes_per_sample(frm)
#define write_next_header(chan,srate,chans,loc,siz,format,comment,len) mus_header_write_next_header(chan,srate,chans,loc,siz,format,comment,len) 
#define set_aifc_header(val) mus_header_set_aifc(val)
#define create_header_buffer() mus_header_initialize()
#define c_set_snd_header(srate,chans,format) mus_header_snd_set_header(srate,chans,format)

#define DEFAULT_DEVICE MUS_AUDIO_DEFAULT
#define READ_WRITE_DEVICE MUS_AUDIO_DUPLEX_DEFAULT
#define ADAT_IN_DEVICE MUS_AUDIO_ADAT_IN
#define AES_IN_DEVICE MUS_AUDIO_AES_IN
#define LINE_OUT_DEVICE MUS_AUDIO_LINE_OUT
#define LINE_IN_DEVICE MUS_AUDIO_LINE_IN
#define MICROPHONE_DEVICE MUS_AUDIO_MICROPHONE
#define SPEAKERS_DEVICE MUS_AUDIO_SPEAKERS
#define DIGITAL_IN_DEVICE MUS_AUDIO_DIGITAL_IN
#define DIGITAL_OUT_DEVICE MUS_AUDIO_DIGITAL_OUT
#define DAC_OUT_DEVICE  MUS_AUDIO_DAC_OUT_
#define ADAT_OUT_DEVICE MUS_AUDIO_ADAT_OUT
#define AES_OUT_DEVICE MUS_AUDIO_AES_OUT
#define DAC_FILTER_DEVICE MUS_AUDIO_DAC_FILTER
#define MIXER_DEVICE MUS_AUDIO_MIXER
#define LINE1_DEVICE MUS_AUDIO_LINE1
#define LINE2_DEVICE MUS_AUDIO_LINE2
#define LINE3_DEVICE MUS_AUDIO_LINE3
#define AUX_INPUT_DEVICE MUS_AUDIO_AUX_INPUT
#define CD_IN_DEVICE MUS_AUDIO_CD
#define AUX_OUTPUT_DEVICE MUS_AUDIO_AUX_OUTPUT
#define SPDIF_IN_DEVICE MUS_AUDIO_SPDIF_IN
#define SPDIF_OUT_DEVICE MUS_AUDIO_SPDIF_OUT

#define NIST_shortpack 2
#define AIFF_IMA_ADPCM 99
#define AUDIO_SYSTEM(n) ((n)<<16)

#ifdef MUS_LITTLE_ENDIAN
  #define COMPATIBLE_FORMAT snd_16_linear_little_endian
#else
  #define COMPATIBLE_FORMAT snd_16_linear
#endif

#ifndef WINDOZE
  #define NO_ERROR 0
#else
  #define NO_ERROR 0L
#endif
#define CHANNELS_NOT_AVAILABLE 1
#define SRATE_NOT_AVAILABLE 2
#define FORMAT_NOT_AVAILABLE 3
#define NO_INPUT_AVAILABLE 4
#define NO_OUTPUT_AVAILABLE 5
#define INPUT_BUSY 6
#define OUTPUT_BUSY 7
#define CONFIGURATION_NOT_AVAILABLE 8
#define INPUT_CLOSED 9
#define OUTPUT_CLOSED 10
#define IO_INTERRUPTED 11
#define NO_LINES_AVAILABLE 12
#define WRITE_ERROR 13
#define SIZE_NOT_AVAILABLE 14
#define DEVICE_NOT_AVAILABLE 15
#define CANT_CLOSE 16
#define CANT_OPEN 17
#define READ_ERROR 18
#define AMP_NOT_AVAILABLE 19
#define AUDIO_NO_OP 20
#define CANT_WRITE 21
#define CANT_READ 22
#define NO_READ_PERMISSION 23

#define AMP_FIELD 0
#define SRATE_FIELD 1
#define CHANNEL_FIELD 2
#define FORMAT_FIELD 3
#define DEVICE_FIELD 4
#define IMIX_FIELD 5
#define IGAIN_FIELD 6
#define RECLEV_FIELD 7
#define PCM_FIELD 8
#define PCM2_FIELD 9
#define OGAIN_FIELD 10
#define LINE_FIELD 11
#define MIC_FIELD 12
#define LINE1_FIELD 13
#define LINE2_FIELD 14
#define LINE3_FIELD 15
#define SYNTH_FIELD 16
#define BASS_FIELD 17
#define TREBLE_FIELD 18
#define CD_FIELD 19

#define MAX_FILE_NAME 128
#define SNDLIB_MAX_FILE_NAME 128

#define snd_unsupported -1
#define snd_no_snd 0
#define snd_16_linear 1
#define snd_8_mulaw 2
#define snd_8_linear 3
#define snd_32_float 4
#define snd_32_linear 5
#define snd_8_alaw 6
#define snd_8_unsigned 7
#define snd_24_linear 8
#define snd_64_double 9
#define snd_16_linear_little_endian 10
#define snd_32_linear_little_endian 11
#define snd_32_float_little_endian 12
#define snd_64_double_little_endian 13
#define snd_16_unsigned 14
#define snd_16_unsigned_little_endian 15
#define snd_24_linear_little_endian 16
#define snd_32_vax_float 17
#define snd_12_linear 18
#define snd_12_linear_little_endian 19
#define snd_12_unsigned 20
#define snd_12_unsigned_little_endian 21

#define old_style_AIFF_sound_file 56

#define set_big_endian_int(j,x) mus_bint_to_char(j,x)
#define get_big_endian_int(in) mus_char_to_bint(in)
#define set_little_endian_int(j,x) mus_lint_to_char(j,x)
#define get_little_endian_int(in) mus_char_to_lint(in)
#define get_uninterpreted_int(in) mus_char_to_uninterpreted_int(in)
#define set_big_endian_float(j,x) mus_bfloat_to_char(j,x)
#define get_big_endian_float(in) mus_char_to_bfloat(in)
#define set_little_endian_float(j,x) mus_lfloat_to_char(j,x)
#define get_little_endian_float(in) mus_char_to_lfloat(in)
#define set_big_endian_short(j,x) mus_bshort_to_char(j,x)
#define get_big_endian_short(in) mus_char_to_bshort(in)
#define set_little_endian_short(j,x) mus_lshort_to_char(j,x)
#define get_little_endian_short(in) mus_char_to_lshort(in)
#define set_big_endian_unsigned_short(j,x) mus_ubshort_to_char(j,x)
#define get_big_endian_unsigned_short(in) mus_char_to_ubshort(in)
#define set_little_endian_unsigned_short(j,x) mus_ulshort_to_char(j,x)
#define get_little_endian_unsigned_short(in) mus_char_to_ulshort(in)
#define get_little_endian_double(in) mus_char_to_ldouble(in)
#define get_big_endian_double(in) mus_char_to_bdouble(in)
#define set_big_endian_double(j,x) mus_bdouble_to_char(j,x)
#define set_little_endian_double(j,x) mus_ldouble_to_char(j,x)
#define get_big_endian_unsigned_int(in) mus_char_to_ubint(in)
#define get_little_endian_unsigned_int(in) mus_char_to_ulint(in)

#define mus_set_datum_type(a,b) mus_file_set_header_type(a,b)
#define mus_set_datum_chans(a,b) mus_file_set_chans(a,b)

#define unsupported_sound_file -1
#define NeXT_sound_file 0
#define AIFC_sound_file 1
#define RIFF_sound_file 2
#define BICSF_sound_file 3
#define NIST_sound_file 4
#define INRS_sound_file 5
#define ESPS_sound_file 6
#define SVX_sound_file 7
#define VOC_sound_file 8
#define SNDT_sound_file 9
#define raw_sound_file 10
#define SMP_sound_file 11
#define SD2_sound_file 12
#define AVR_sound_file 13
#define IRCAM_sound_file 14
#define SD1_sound_file 15
#define SPPACK_sound_file 16
#define MUS10_sound_file 17
#define HCOM_sound_file 18
#define PSION_sound_file 19
#define MAUD_sound_file 20
#define IEEE_sound_file 21
#define DeskMate_sound_file 22
#define DeskMate_2500_sound_file 23
#define Matlab_sound_file 24
#define ADC_sound_file 25
#define SoundEdit_sound_file 26
#define SoundEdit_16_sound_file 27
#define DVSM_sound_file 28
#define MIDI_file 29
#define Esignal_file 30
#define soundfont_sound_file 31
#define gravis_sound_file 32
#define comdisco_sound_file 33
#define goldwave_sound_file 34
#define srfs_sound_file 35
#define MIDI_sample_dump 36
#define DiamondWare_sound_file 37
#define RealAudio_sound_file 38
#define ADF_sound_file 39
#define SBStudioII_sound_file 40
#define Delusion_sound_file 41
#define Farandole_sound_file 42
#define Sample_dump_sound_file 43
#define Ultratracker_sound_file 44
#define Yamaha_SY85_sound_file 45
#define Yamaha_TX16_sound_file 46
#define digiplayer_sound_file 47
#define Covox_sound_file 48
#define SPL_sound_file 49
#define AVI_sound_file 50
#define OMF_sound_file 51
#define Quicktime_sound_file 52
#define asf_sound_file 53
#define Yamaha_SY99_sound_file 54
#define Kurzweil_2000_sound_file 55
#define AIFF_sound_file 56

#define mus_shift_24_choice() 0
#define mus_set_shift_24_choice(n) n
#define list_ptr(n) mus_ptr2table(n)
#define delist_ptr(n) mus_table2ptr(n)

#define SNDLIB_LITTLE_ENDIAN MUS_LITTLE_ENDIAN
#define SNDLIB_SAMPLE_TYPE MUS_SAMPLE_TYPE
#define SNDLIB_SAMPLE_BITS MUS_SAMPLE_BITS
#define SNDLIB_SAMPLE_0 MUS_SAMPLE_0

#define SNDLIB_BYTE_TO_SAMPLE(n) MUS_BYTE_TO_SAMPLE(n)
#define SNDLIB_SAMPLE_TO_BYTE(n) MUS_SAMPLE_TO_BYTE(n)
#define SNDLIB_SHORT_TO_SAMPLE(n) MUS_SHORT_TO_SAMPLE(n)
#define SNDLIB_SAMPLE_TO_SHORT(n) MUS_SAMPLE_TO_SHORT(n)
#define SNDLIB_INT24_TO_SAMPLE(n) MUS_INT24_TO_SAMPLE(n)
#define SNDLIB_SAMPLE_TO_INT24(n) MUS_SAMPLE_TO_INT24(n)
#define SNDLIB_INT_TO_SAMPLE(n) MUS_INT_TO_SAMPLE(n)
#define SNDLIB_SAMPLE_TO_INT(n) MUS_SAMPLE_TO_INT(n)
#define SNDLIB_SNDFIX MUS_SNDFIX
#define SNDLIB_SNDFLT MUS_SNDFLT
#define SNDLIB_FLOAT_TO_SAMPLE(n) MUS_FLOAT_TO_SAMPLE(n)
#define SNDLIB_SAMPLE_TO_FLOAT(n) MUS_SAMPLE_TO_FLOAT(n)
#define SNDLIB_DOUBLE_TO_SAMPLE(n) MUS_DOUBLE_TO_SAMPLE(n)
#define SNDLIB_SAMPLE_TO_DOUBLE(n) MUS_SAMPLE_TO_DOUBLE(n)
#define SNDLIB_SAMPLE_MAX MUS_SAMPLE_MAX
#define SNDLIB_SAMPLE_MIN MUS_SAMPLE_MIN
#define SNDLIB_MIX_MAX MUS_MIX_MAX
#define SNDLIB_MIX_MIN MUS_MIX_MIN

#define SNDLIB_DAC_CHANNEL MUS_DAC_CHANNEL
#define SNDLIB_DAC_REVERB MUS_DAC_REVERB

#define SNDLIB_HDR_UNSUPPORTED -1
enum {SNDLIB_HDR_NEXT,SNDLIB_HDR_AIFC,SNDLIB_HDR_RIFF,SNDLIB_HDR_BICSF,SNDLIB_HDR_NIST,SNDLIB_HDR_INRS,SNDLIB_HDR_ESPS,
	SNDLIB_HDR_SVX,SNDLIB_HDR_VOC,SNDLIB_HDR_SNDT,SNDLIB_HDR_RAW,SNDLIB_HDR_SMP,SNDLIB_HDR_SD2,SNDLIB_HDR_AVR,
	SNDLIB_HDR_IRCAM,SNDLIB_HDR_SD1,SNDLIB_HDR_SPPACK,SNDLIB_HDR_MUS10,SNDLIB_HDR_HCOM,SNDLIB_HDR_PSION,SNDLIB_HDR_MAUD,
	SNDLIB_HDR_IEEE,SNDLIB_HDR_DESKMATE,SNDLIB_HDR_DESKMATE_2500,SNDLIB_HDR_MATLAB,SNDLIB_HDR_ADC,SNDLIB_HDR_SOUND_EDIT,
	SNDLIB_HDR_SOUND_EDIT_16,SNDLIB_HDR_DVSM,SNDLIB_HDR_MIDI,SNDLIB_HDR_ESIGNAL,SNDLIB_HDR_SOUNDFONT,SNDLIB_HDR_GRAVIS,
	SNDLIB_HDR_COMDISCO,SNDLIB_HDR_GOLDWAVE,SNDLIB_HDR_SRFS,SNDLIB_HDR_MIDI_SAMPLE_DUMP,SNDLIB_HDR_DIAMONDWARE,
	SNDLIB_HDR_REALAUDIO,SNDLIB_HDR_ADF,SNDLIB_HDR_SBSTUDIOII,SNDLIB_HDR_DELUSION,SNDLIB_HDR_FARANDOLE,SNDLIB_HDR_SAMPLE_DUMP,
	SNDLIB_HDR_ULTRATRACKER,SNDLIB_HDR_YAMAHA_SY85,SNDLIB_HDR_YAMAHA_TX16,SNDLIB_HDR_DIGIPLAYER,SNDLIB_HDR_COVOX,SNDLIB_HDR_SPL,
	SNDLIB_HDR_AVI,SNDLIB_HDR_OMF,SNDLIB_HDR_QUICKTIME,SNDLIB_HDR_ASF,SNDLIB_HDR_YAMAHA_SY99,SNDLIB_HDR_KURZWEIL_2000,
      SNDLIB_HDR_AIFF,SNDLIB_HDR_PAF};

#define SNDLIB_HEADER_TYPE_OK(n) MUS_HEADER_TYPE_OK(n)

#define SNDLIB_UNSUPPORTED -1
enum {SNDLIB_NO_SND,SNDLIB_16_LINEAR,SNDLIB_8_MULAW,SNDLIB_8_LINEAR,SNDLIB_32_FLOAT,SNDLIB_32_LINEAR,SNDLIB_8_ALAW,SNDLIB_8_UNSIGNED,
	SNDLIB_24_LINEAR,SNDLIB_64_DOUBLE,SNDLIB_16_LINEAR_LITTLE_ENDIAN,SNDLIB_32_LINEAR_LITTLE_ENDIAN,SNDLIB_32_FLOAT_LITTLE_ENDIAN,
	SNDLIB_64_DOUBLE_LITTLE_ENDIAN,SNDLIB_16_UNSIGNED,SNDLIB_16_UNSIGNED_LITTLE_ENDIAN,SNDLIB_24_LINEAR_LITTLE_ENDIAN,SNDLIB_32_VAX_FLOAT,
      SNDLIB_12_LINEAR,SNDLIB_12_LINEAR_LITTLE_ENDIAN,SNDLIB_12_UNSIGNED,SNDLIB_12_UNSIGNED_LITTLE_ENDIAN};

#define SNDLIB_DATA_FORMAT_OK(n), MUS_DATA_FORMAT_OK(n),

#define SNDLIB_COMPATIBLE_FORMAT MUS_COMPATIBLE_FORMAT
#define SNDLIB_OUT_FORMAT MUS_OUT_FORMAT

#define SNDLIB_NIST_shortpack MUS_NIST_shortpack
#define SNDLIB_AIFF_IMA_ADPCM MUS_AIFF_IMA_ADPCM

enum {SNDLIB_NO_ERROR,SNDLIB_CHANNELS_NOT_AVAILABLE,SNDLIB_SRATE_NOT_AVAILABLE,SNDLIB_FORMAT_NOT_AVAILABLE,SNDLIB_NO_INPUT_AVAILABLE,
      SNDLIB_NO_OUTPUT_AVAILABLE,SNDLIB_INPUT_BUSY,SNDLIB_OUTPUT_BUSY,SNDLIB_CONFIGURATION_NOT_AVAILABLE,SNDLIB_INPUT_CLOSED,
      SNDLIB_OUTPUT_CLOSED,SNDLIB_IO_INTERRUPTED,SNDLIB_NO_LINES_AVAILABLE,SNDLIB_WRITE_ERROR,SNDLIB_SIZE_NOT_AVAILABLE,SNDLIB_DEVICE_NOT_AVAILABLE,
      SNDLIB_CANT_CLOSE,SNDLIB_CANT_OPEN,SNDLIB_READ_ERROR,SNDLIB_AMP_NOT_AVAILABLE,SNDLIB_AUDIO_NO_OP,SNDLIB_CANT_WRITE,SNDLIB_CANT_READ,
      SNDLIB_NO_READ_PERMISSION};

#define SNDLIB_AUDIO_SYSTEM(n) MUS_AUDIO_PACK_SYSTEM(n)
#define SNDLIB_SYSTEM(n) MUS_AUDIO_SYSTEM(n)
#define SNDLIB_DEVICE(n) MUS_AUDIO_DEVICE(n)

#define SNDLIB_DEFAULT_DEVICE MUS_AUDIO_DEFAULT
#define SNDLIB_READ_WRITE_DEVICE MUS_AUDIO_DUPLEX_DEFAULT
#define SNDLIB_ADAT_IN_DEVICE MUS_AUDIO_ADAT_IN
#define SNDLIB_AES_IN_DEVICE MUS_AUDIO_AES_IN
#define SNDLIB_LINE_OUT_DEVICE MUS_AUDIO_LINE_OUT
#define SNDLIB_LINE_IN_DEVICE MUS_AUDIO_LINE_IN
#define SNDLIB_MICROPHONE_DEVICE MUS_AUDIO_MICROPHONE
#define SNDLIB_SPEAKERS_DEVICE MUS_AUDIO_SPEAKERS
#define SNDLIB_DIGITAL_IN_DEVICE MUS_AUDIO_DIGITAL_IN
#define SNDLIB_DIGITAL_OUT_DEVICE MUS_AUDIO_DIGITAL_OUT
#define SNDLIB_DAC_OUT_DEVICE MUS_AUDIO_DAC_OUT
#define SNDLIB_ADAT_OUT_DEVICE MUS_AUDIO_ADAT_OUT
#define SNDLIB_AES_OUT_DEVICE MUS_AUDIO_AES_OUT
#define SNDLIB_DAC_FILTER_DEVICE MUS_AUDIO_DAC_FILTER
#define SNDLIB_MIXER_DEVICE MUS_AUDIO_MIXER
#define SNDLIB_LINE1_DEVICE MUS_AUDIO_LINE1
#define SNDLIB_LINE2_DEVICE MUS_AUDIO_LINE2
#define SNDLIB_LINE3_DEVICE MUS_AUDIO_LINE3
#define SNDLIB_AUX_INPUT_DEVICE MUS_AUDIO_AUX_INPUT
#define SNDLIB_CD_IN_DEVICE MUS_AUDIO_CD
#define SNDLIB_AUX_OUTPUT_DEVICE MUS_AUDIO_AUX_OUTPUT
#define SNDLIB_SPDIF_IN_DEVICE MUS_AUDIO_SPDIF_IN
#define SNDLIB_SPDIF_OUT_DEVICE MUS_AUDIO_SPDIF_OUT

#define SNDLIB_AMP_FIELD MUS_AUDIO_AMP
#define SNDLIB_SRATE_FIELD MUS_AUDIO_SRATE
#define SNDLIB_CHANNEL_FIELD MUS_AUDIO_CHANNEL
#define SNDLIB_FORMAT_FIELD MUS_AUDIO_FORMAT
#define SNDLIB_DEVICE_FIELD MUS_AUDIO_PORT
#define SNDLIB_IMIX_FIELD MUS_AUDIO_IMIX
#define SNDLIB_IGAIN_FIELD MUS_AUDIO_IGAIN
#define SNDLIB_RECLEV_FIELD MUS_AUDIO_RECLEV
#define SNDLIB_PCM_FIELD MUS_AUDIO_PCM
#define SNDLIB_PCM2_FIELD MUS_AUDIO_PCM2
#define SNDLIB_OGAIN_FIELD MUS_AUDIO_OGAIN
#define SNDLIB_LINE_FIELD MUS_AUDIO_LINE
#define SNDLIB_SYNTH_FIELD MUS_AUDIO_SYNTH
#define SNDLIB_BASS_FIELD MUS_AUDIO_BASS
#define SNDLIB_TREBLE_FIELD MUS_AUDIO_TREBLE
#define SNDLIB_CD_FIELD MUS_AUDIO_CD

#define SNDLIB_SAMPLE_ARRAY(n) MUS_SAMPLE_ARRAY(n)
#define SNDLIB_MAKE_SAMPLE_ARRAY(size) MUS_MAKE_SAMPLE_ARRAY(n)
#define SNDLIB_FREE_SAMPLE_ARRAY(p) MUS_FREE_SAMPLE_ARRAY(n)

#define mus_set_error_handler(n) mus_error_set_handler(n)
#define mus_make_error_tag(n) mus_error_make_tag(n)
#define sound_samples(n) mus_sound_samples(n)
#define sound_frames(n) mus_sound_frames(n)
#define sound_datum_size(n) mus_sound_datum_size(n)
#define sound_data_location(n) mus_sound_data_location(n)
#define sound_chans(n) mus_sound_chans(n)
#define sound_srate(n) mus_sound_srate(n)
#define sound_header_type(n) mus_sound_header_type(n)
#define sound_data_format(n) mus_sound_data_format(n)
#define sound_original_format(n) mus_sound_original_format(n)
#define sound_comment_start(n) mus_sound_comment_start(n)
#define sound_comment_end(n) mus_sound_comment_end(n)
#define sound_length(n) mus_sound_length(n)
#define sound_fact_samples(n) mus_sound_fact_samples(n)
#define sound_distributed(n) mus_sound_distributed(n)
#define sound_write_date(n) mus_sound_write_date(n)
#define sound_type_specifier(n) mus_sound_type_specifier(n)
#define sound_align(n) mus_sound_align(n)
#define sound_bits_per_sample(n) mus_sound_bits_per_sample(n)
#define sound_type_name(n) mus_header_type_name(n)
#define sound_format_name(n) mus_data_format_name(n)
#define sound_comment(n) mus_sound_comment(n)
#define sound_bytes_per_sample(n) mus_data_format_to_bytes_per_sample(n)
#define sound_duration(n) mus_sound_duration(n)
#define initialize_sndlib() mus_sound_initialize()
#define finalize_sndlib(n) mus_sound_finalize()
#define sndlib_sample_bits() mus_sample_bits()
#define override_sound_header(a,b,c,d,e,f,g) mus_sound_override_header(a,b,c,d,e,f,g)
#define forget_sound(n) mus_sound_forget(n)
#define sound_print_cache() mus_sound_print_cache()
#define sound_aiff_p(n) mus_sound_aiff_p(n)
#define sound_loop_info(n) mus_sound_loop_info(n)
#define sound_set_loop_info(n) mus_sound_set_loop_info(n)
#define open_sound_input(n) mus_sound_open_input(n)
#define open_sound_output(a,b,c,d,e,f) mus_sound_open_output(a,b,c,d,e,f)
#define reopen_sound_output(a,b,c,d,e) mus_sound_reopen_output(a,b,c,d,e)
#define close_sound_input(n) mus_sound_close_input(n)
#define close_sound_output(a,b) mus_sound_close_output(a,b)
#define read_sound(a,b,c,d,e) mus_sound_read(a,b,c,d,e)
#define write_sound(a,b,c,d,e) mus_sound_write(a,b,c,d,e)
#define seek_sound(a,b,c) mus_sound_seek(a,b,c)
#define seek_sound_frame(a,b) mus_sound_seek_frame(a,b)
#define sound_max_amp(a,b) mus_sound_max_amp(a,b)
#define mus_file2array(a,b,c,d,e) mus_file_to_array(a,b,c,d,e)
#define mus_array2file(a,b,c,d,e) mus_array_to_file(a,b,c,d,e)
#define describe_audio_state() mus_audio_describe()
#define report_audio_state() mus_audio_report()
#define open_audio_output(a,b,c,d,e) mus_audio_open_output(a,b,c,d,e)
#define open_audio_input(a,b,c,d,e) mus_audio_open_input(a,b,c,d,e)
#define write_audio(a,b,c) mus_audio_write(a,b,c)
#define close_audio(n) mus_audio_close(n)
#define read_audio(a,b,c) mus_audio_read(a,b,c)
#define read_audio_state(a,b,c,d) mus_audio_mixer_read(a,b,c,d)
#define write_audio_state(a,b,c,d) mus_audio_mixer_write(a,b,c,d)
#define save_audio_state() mus_audio_save()
#define restore_audio_state() mus_audio_restore()
#define audio_error() mus_audio_error()
#define initialize_audio() mus_audio_initialize()
#define audio_error_name(n) mus_audio_error_name(n)
#define set_audio_error(n) mus_audio_set_error(n)
#define audio_systems() mus_audio_systems()
#define audio_system_name(n) mus_audio_system_name(n)
#define audio_moniker() mus_audio_moniker()
#define set_dsp_devices(a,b,c) mus_audio_set_dsp_devices(a,b,c)
#define dsp_devices(a,b,c) mus_audio_dsp_devices(a,b,c)
#define set_oss_buffers(a,b) mus_audio_set_oss_buffers(a,b)
#define write_mixer_state(n) mus_audio_mixer_save(n)
#define read_mixer_state(n) mus_audio_mixer_restore(n)
#define sun_outputs(a,b,c) mus_audio_sun_outputs(a,b,c)
#define clear_soundcard_inputs() mus_audio_clear_soundcard_inputs()
#define mus_open_file_descriptors(a,b,c,d) mus_file_open_descriptors(a,b,c,d)
#define mus_set_file_descriptors(a,b,c,d,e,f,g) mus_file_set_descriptors(a,b,c,d,e,f,g)
#define mus_close_file_descriptors(n) mus_file_close_descriptors(n)
#define mus_cleanup_file_descriptors() mus_file_cleanup_descriptors()
#define mus_open_read(n) mus_file_open_read(n)
#define mus_probe_file(n) mus_file_probe(n)
#define mus_open_write(n) mus_file_open_write(n)
#define mus_create(n) mus_file_create(n)
#define mus_reopen_write(n) mus_file_reopen_write(n)
#define mus_close(n) mus_file_close(n)
#define mus_seek(a,b,c) mus_file_seek(a,b,c)
#define mus_seek_frame(a,b,c) mus_file_seek_frame(a,b,c)
#define mus_read(a,b,c,d,e) mus_file_read(a,b,c,d,e)
#define mus_read_chans(a,b,c,d,e,f) mus_file_read_chans(a,b,c,d,e,f)
#define mus_write_zeros(a,b) mus_file_write_zeros(a,b)
#define mus_write(a,b,c,d,e) mus_file_write(a,b,c,d,e)
#define mus_create_descriptors()
#define mus_file_create_descriptors()
#define mus_read_any(a,b,c,d,e,f) mus_file_read_any(a,b,c,d,e,f)
#define mus_read_from_file(a,b,c,d,e) mus_file_read_file(a,b,c,d,e)
#define mus_read_from_buffer(a,b,c,d,e,f) mus_file_read_buffer(a,b,c,d,e,f)
#define mus_write_to_file(a,b,c,d,e) mus_file_write_file(a,b,c,d,e)
#define mus_write_to_buffer(a,b,c,d,e,f,g) mus_file_write_buffer(a,b,c,d,e,f,g)
#define mus_complete_filename(n) mus_file_full_name(n)
#define mus_set_data_clipped(a,b) mus_file_set_data_clipped(a,b)
#define mus_set_header_type(a,b) mus_file_set_header_type(a,b)
#define mus_get_header_type(n) mus_file_header_type(n)
#define mus_fd2file_name(n) mus_file_fd_name(n)
#define mus_set_chans(a,b) mus_file_set_chans(a,b)
#define mus_prescaler(n) mus_file_prescaler(n)
#define mus_set_prescaler(a,b) mus_file_set_prescaler(a,b)
#define mus_set_raw_header_defaults(a,b,c) mus_header_set_raw_defaults(a,b,c)
#define mus_true_file_length() mus_header_true_length()
#define mus_format2bytes() mus_header_data_format_to_bytes_per_sample()
#define mus_header_format2bytes() mus_header_data_format_to_bytes_per_sample()
#define mus_samples2bytes(a,b) mus_samples_to_bytes(a,b)
#define mus_bytes2samples(a,b) mus_bytes_to_samples(a,b)
#define mus_write_next_header(a,b,c,d,e,f,g,h) mus_header_write_next_header(a,b,c,d,e,f,g,h)
#define mus_read_header_with_fd(n) mus_header_read_with_fd(n)
#define mus_read_header(n) mus_header_read(n)
#define mus_write_header(a,b,c,d,e,f,g,h,i) mus_header_write(a,b,c,d,e,f,g,h,i)
#define mus_write_header_with_fd(a,b,c,d,e,f,g,h,i) mus_header_write_with_fd(a,b,c,d,e,f,g,h,i)
#define mus_update_header_with_fd(a,b,c) mus_header_update_with_fd(a,b,c)
#define mus_update_header(a,b,c,d,e,f,g) mus_header_update(a,b,c,d,e,f,g)
#define mus_update_header_comment(a,b,c,d,e) mus_header_update_comment(a,b,c,d,e)
#define mus_create_header_buffer() mus_header_initialize()
#define mus_set_snd_header(a,b,c) mus_header_snd_set_header(a,b,c)
#define mus_set_big_endian_int(a,b) mus_bint_to_char(a,b)
#define mus_big_endian_int(n) mus_char_to_bint(n)
#define mus_set_little_endian_int(a,b) mus_lint_to_char(a,b)
#define mus_little_endian_int(n) mus_char_to_lint(n)
#define mus_uninterpreted_int(n) mus_char_to_uninterpreted_int(n)
#define mus_set_big_endian_float(a,b) mus_bfloat_to_char(a,b)
#define mus_big_endian_float(n) mus_char_to_bfloat(n)
#define mus_set_little_endian_float(a,b) mus_lfloat_to_char(a,b)
#define mus_little_endian_float(n) mus_char_to_lfloat(n)
#define mus_set_big_endian_short(a,b) mus_bshort_to_char(a,b)
#define mus_big_endian_short(n) mus_char_to_bshort(n)
#define mus_set_little_endian_short(a,b) mus_lshort_to_char(a,b)
#define mus_little_endian_short(n) mus_char_to_lshort(n)
#define mus_set_big_endian_unsigned_short(a,b) mus_ubshort_to_char(a,b)
#define mus_big_endian_unsigned_short(n) mus_char_to_ubshort(n)
#define mus_set_little_endian_unsigned_short(a,b) mus_ulshort_to_char(a,b)
#define mus_little_endian_unsigned_short(n) mus_char_to_ulshort(n)
#define mus_little_endian_double(n) mus_char_to_ldouble(n)
#define mus_big_endian_double(n) mus_char_to_bdouble(n)
#define mus_set_big_endian_double(a,b) mus_bdouble_to_char(a,b)
#define mus_set_little_endian_double(a,b) mus_ldouble_to_char(a,b)
#define mus_big_endian_unsigned_int(n) mus_char_to_ubint(n)
#define mus_little_endian_unsigned_int(n) mus_char_to_ulint(n)
#define mus_set_aifc_header(n) mus_header_set_aifc(n)
#define init_sndlib2scm() mus_sndlib2scm_initialize()

#endif



/* translate various special case sound files to something we can edit 
 * 
 * I'm ignoring propietary or licensed schemes even where the code is publicly available (Rockwell ADPCM, shorten, etc)
 *
 * currently supported:
 *   IEEE text
 *   Mus10 16-bit SAM (mode 4)
 *   IBM CVSD RIFF
 *   HCOM (from Sox)
 *   shortpack NIST
 *   Dvi-Intel (IMA) ADPCM RIFF (comes in 3 and 4 bit flavors, but just 4-bit here) (MS and Apple are variations of this)
 *   MIDI sample dump
 *   Oki (Dialogic) ADPCM (RIFF)
 *   Yamaha TX-16 12-bit
 *   IFF Fibonacci and Exponential (untested)
 *   NeXT/Sun G721, G723 3 and 5 bit versions (also RIFF) (AIFC case could be handled if they exist)
 */

#include "snd.h"

#define TRANS_BUF_SIZE 8192

static char write_error_buffer[256];

static int snd_checked_write(int fd, unsigned char *buf, int bytes)
{
  /* io.c checked_write assumes its file descriptors are around */
  /* can't call mus_error here because we need to clean up first in case of error */
  int bytes_written,kfree;
  kfree = disk_kspace(fd);
  if (kfree < 0) 
    {
      sprintf(write_error_buffer,
	      "no space left on device: %s",
	      strerror(errno)); 
      return(MUS_ERROR);
    }
  if (kfree < (bytes>>10))
    { 
      sprintf(write_error_buffer,
	      "only %d bytes left on device (we need %d bytes)",
	      kfree<<10,bytes);
      return(MUS_ERROR);
    }
  bytes_written = write(fd,buf,bytes);
  if (bytes_written != bytes)
    {
      sprintf(write_error_buffer,
	      "write error (wrote %d of requested %d bytes): %s",
	      bytes_written,bytes,strerror(errno));
      return(MUS_ERROR);
    }
  return(bytes_written);
}

static int be_snd_checked_write(int fd, unsigned char *buf, int bytes)
{
  /* handle little-endian swap if necessary */
#ifdef MUS_LITTLE_ENDIAN
  unsigned char tmp;
  int i;
  for (i=0;i<bytes;i+=2)
    {
      tmp = buf[i];
      buf[i] = buf[i+1];
      buf[i+1] = tmp;
    }
#endif
  return(snd_checked_write(fd,buf,bytes));
}

#define RETURN_MUS_IO_ERROR(IO_Func,IO_Name) \
  do { \
      mus_error(MUS_CANT_OPEN_FILE,"translator: %s(%s): %s\n  [%s[%d] %s]", \
                IO_Func,IO_Name,strerror(errno), \
                __FILE__,__LINE__,__FUNCTION__); \
      return(MUS_ERROR); \
      } \
  while (0)

#define RETURN_MUS_WRITE_ERROR(OldName,NewName) \
  do { \
      mus_error(MUS_WRITE_ERROR,"can't translate %s to %s:\n  %s\n [snd-trans.c[%d] %s]", \
		OldName,NewName,write_error_buffer, \
		__LINE__,__FUNCTION__); \
      write_error_buffer[0] = '\0'; \
      return(MUS_ERROR); \
    } \
  while (0)

#define RETURN_MUS_ALLOC_ERROR(OldName,Bytes,VariableName) \
  do { \
      mus_error(MUS_MEMORY_ALLOCATION_FAILED,"translate %s: can't allocate %d bytes for %s:\n  [snd-trans.c[%d] %s]", \
		OldName,Bytes,VariableName, \
		__LINE__,__FUNCTION__); \
      return(MUS_ERROR); \
    } \
  while (0)


/* I'm using the same variable names in most cases below, so these two macros save lots of repetition */
#define CLEANUP() \
  do { \
      if (fs != -1) close(fs); \
      if (fd != -1) close(fd); \
      if (buf) FREE(buf); \
     } \
  while (0)

#define STARTUP(OldName,NewName,BufSize,BufType) \
  do { \
    fs = creat(NewName,0666); \
    if (fs == -1) RETURN_MUS_IO_ERROR("create",NewName); \
    fd = open(OldName,O_RDONLY,0); \
    if (fd == -1) \
      { \
        CLEANUP(); \
        RETURN_MUS_IO_ERROR("open",OldName); \
      } \
    buf = (BufType *)CALLOC(BufSize,sizeof(BufType)); \
    if (buf == NULL) \
      { \
        CLEANUP(); \
        RETURN_MUS_ALLOC_ERROR(OldName,BufSize,"buf"); \
      } \
    } \
  while (0)


/* -------------------------------- MIDI sample dump -------------------------------- */

/* F0 7E <ID> 01 ss ss ee ff ff ff gg gg gg hh hh hh ii ii ii jj f7
 * ss: sample# (LSB MSB), ee: #bits, ff: 1/srate in nsec, gg: samples, hh: loop ii: loop jj: loop 
 * 0000000       f07e 0001 0000 1048 5007 7479 0000 0000
 * 0000020       0000 007f f7f0 7e00 0200 4000 003f 7140
 */

static int read_midi_sample_dump(char *oldname, char *newname, char *hdr)
{
  int fs=-1,fd=-1,err=MUS_NO_ERROR,totalin,happy,chans,srate,inp,outp;
  int val = 0,bits,block_count,header_count,state,samples,shift1,shift2,offset;
  int osp;
  unsigned char *buf = NULL;
  chans = 1;
  STARTUP(oldname,newname,TRANS_BUF_SIZE,unsigned char);
  totalin = read(fd,buf,TRANS_BUF_SIZE);
  bits = buf[6];
  srate = (int)(1.0e9 / (float)((buf[7] + (buf[8]<<7) + (buf[9]<<14))));
  samples = (buf[10] + (buf[11]<<7) + (buf[12]<<14));
  mus_bint_to_char((unsigned char *)(hdr+16),srate);
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  if (bits == 16) 
    mus_bint_to_char((unsigned char *)(hdr+8),samples*2); 
  else mus_bint_to_char((unsigned char *)(hdr+8),samples);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR)
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  happy = 1;
  inp = 21;
  block_count = 120;
  state = 2;
  header_count = 5;
  outp = 0;
  osp = 0;
  /* we could be really wacked out and implement any sample width here */
  if (bits == 16) 
    {
      shift1 = 9; 
      shift2 = 5;
      offset = 32768;
    }
  else 
    {
      shift1 = 1;
      shift2 = 6;
      offset = 128;
    }
  while (happy)
    {
      if (inp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = 0;
	  else 
	    {
	      totalin = read(fd,buf,TRANS_BUF_SIZE); 
	      inp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs,(unsigned char *)hdr,TRANS_BUF_SIZE) == MUS_ERROR) 
	    {
	      CLEANUP();
	      RETURN_MUS_WRITE_ERROR(oldname,newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  if (state != 2) 
	    {
	      block_count--; 
	      if (block_count == 0) 
		{
		  state = 2; 
		  header_count = 7;
		}
	    }
	  switch (state)
	    {
	    case 0: 
	      /* val = buf[inp];  */
	      /* hmmm...  I wonder about this -- the MIDI spec says LSB first,
	       *   but the Goldwave midi sample dump output sends MSB first.
	       * I bet this is a bug 
	       */
	      val = buf[inp] << shift1;
	      state = ((bits == 16) ? 1 : 3); 
	      break;
	    case 1: 
	      /* val |= (buf[inp] << 7);  */
	      val |= (buf[inp] << 2);
	      state = 3; 
	      break;
	    case 2: 
	      header_count--; 
	      if (header_count == 0) 
		{
		  state = 0; 
		  block_count = 121;
		} 
	      break;
	    case 3: 
	      /* val |= (buf[inp] << shift1);  */
	      val |= (buf[inp] >> shift2);
	      state = 0; 
	      mus_bshort_to_char((unsigned char *)(hdr+osp),val-offset);
	      osp+=2;
	      outp+=2; 
	      break;
	    }
	  inp++;
	}
    }
  if (outp > 0) err = snd_checked_write(fs,(unsigned char *)hdr,outp);
  CLEANUP();
  if (err == MUS_ERROR) RETURN_MUS_WRITE_ERROR(oldname,newname);
  return(MUS_NO_ERROR);
}



/* -------------------------------- IEEE TEXT -------------------------------- */

static int read_ieee_text(char *oldname, char *newname, char *hdr)
{
  /* from untext.c */
  /* look for "%sampling rate: nn.nn KHz\n", also get end of to comment (i.e. data location) */
  char str[32];
  char *buf=NULL;
  int fd=-1,fs=-1,totalin;
  int commenting,inp,outp,op,happy,i,j,s0,srate,err=MUS_NO_ERROR;
  float fsrate;
  int osp;
  STARTUP(oldname,newname,TRANS_BUF_SIZE,char);
  totalin = read(fd,buf,TRANS_BUF_SIZE);      
  commenting = 1;
  inp = 0;
  outp = 24;
  srate = 0;
  op = 0;
  if (buf[0] != '%') 
    {
      CLEANUP();
      mus_error(MUS_UNSUPPORTED_DATA_FORMAT,
		"can't translate IEEE text file %s:\n  Expected initial \"%%\" but found \"%c\"\n  [snd-trans.c[%d] %s]",
		oldname,buf[0],__LINE__,__FUNCTION__);
      return(MUS_ERROR);
    }
  while (commenting)
    {
      if (buf[inp] == '%') {op = inp; inp++;}
      else
	{
	  if (buf[inp] == '\n')
	    {
	      if (srate == 0)
		{
		  for (i=op+1,j=0;(i<inp) && (j < 13);i++,j++) str[j] = buf[i];
		  str[13] = '\0';
		  if (strcmp(str,"sampling rate") == 0) 
		    {
		      for (i=op+15,j=0;j<6;i++,j++) str[j] = buf[i];
		      str[6] = '\0';
		      sscanf(str,"%f",&fsrate);
		      srate = (int)(fsrate*1000);
		    }
		  else
		    {
		      if (strcmp(str,"Sampling Rate") == 0)
			{
			  for (i=op+15,j=0;j<6;i++,j++) str[j] = buf[i];
			  str[6] = '\0';
			  sscanf(str,"%d",&srate);
			}
		    }
		}
	      inp++;
	      if (buf[inp] != '%') commenting = 0;
	      else
		{
		  hdr[outp] = '\n';
		  outp++;
		}
	    }
	  else
	    {
	      hdr[outp] = buf[inp];
	      outp++;
	      inp++;
	    }
	}
    }
  i=(outp%4);
  outp += i;
  mus_bint_to_char((unsigned char *)(hdr+4),outp);
  if (srate != 0) mus_bint_to_char((unsigned char *)(hdr+16),srate);
  if (snd_checked_write(fs,(unsigned char *)hdr,outp) == MUS_ERROR) 
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  happy = 1;
  s0 = 0;
  outp = 0;
  osp = 0;
  while (happy)
    {
      if (inp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = 0;
	  else 
	    {
	      totalin = read(fd,buf,TRANS_BUF_SIZE); 
	      inp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs,(unsigned char *)hdr,TRANS_BUF_SIZE) == MUS_ERROR) 
	    {
	      CLEANUP();
	      RETURN_MUS_WRITE_ERROR(oldname,newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  if (buf[inp] == '\n')
	    {
	      str[s0] = '\0';
	      sscanf(str,"%d",&j);
	      mus_bshort_to_char((unsigned char *)(hdr+osp),j);
	      osp += 2;
	      outp += 2;
	      inp++;
	      s0 = 0;
	    }
	  else
	    {
	      str[s0] = buf[inp];
	      s0++;
	      inp++;
	    }
	}
    }
  err = snd_checked_write(fs,(unsigned char *)hdr,outp);
  /* update size field? */
  CLEANUP();
  if (err == MUS_ERROR) RETURN_MUS_WRITE_ERROR(oldname,newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- Mus10 -------------------------------- */

#define PDP_BUF_SIZE (9*1024)

static int read_mus10(char *oldname, char *newname, char *hdr)
{
  /* from trans.lisp */
  /* nostalgic code -- 36 bit words, two 16-bit samples, right justified */
  /* or (even more archaeological) 12 bits packed 3 to a 36-bit word */
  unsigned char *buf=NULL;
  int fd=-1,fs=-1,totalin,inp,outp,happy,val,err=MUS_NO_ERROR;
  int osp;
  float fsrate,fraction;
  int srateH,srateL,sign,exponent,chans,mode;
  STARTUP(oldname,newname,PDP_BUF_SIZE,unsigned char);
  totalin = read(fd,buf,PDP_BUF_SIZE);      
  /* read the PDP-10 float srate, nchans, mode, etc */
  /* old header started with 36 bits of 0xaaaaaaaaa */
  srateH = (((buf[4] & 0xF) << 14) | (buf[5]<<6) | (buf[6]>>2));
  srateL = (((buf[6] & 0x3) << 16) | (buf[7]<<8) | (buf[8]));
  /* PDP-10 floating point format was sign in bit 0 , excess 128 exponent in 1-8, fraction in 9-35 */
  if (srateH & 0400000) sign = -1; else sign = 1;
  exponent = ((srateH & 0377000)>>9) - 128;
  fraction = (float)(((srateH & 0777)<<18) | srateL) / pow(2.0,27);
  fsrate = sign * pow(2.0,exponent) * fraction;
  if (fsrate > 6400.0) 
    mus_bint_to_char((unsigned char *)(hdr+16),(int)fsrate);  
  else
    {
      /* perhaps old style header? */
      if (srateH != 0) mus_bint_to_char((unsigned char *)(hdr+16),srateH);
    }
  mode = ((buf[11] & 0x3F)<<12) | (buf[12]<<4) | (buf[13]>>4);
  chans = ((buf[15] & 0x3)<<12) | (buf[16]<<8) | buf[17];
  if (chans == 0) chans = 1;
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  if ((mode != 4) && (mode != 0)) 
    {
      CLEANUP();
      mus_error(MUS_UNSUPPORTED_DATA_FORMAT,
		"can't translate Mus10 file %s:\n  mode = %d\n  [snd-trans.c[%d] %s]",
		oldname,mode,__LINE__,__FUNCTION__);
      return(MUS_ERROR);
    }
  /* 4 = SAM 16-bit packing mode, 0 = 12 bit 3 to a word */
  /* now jump to data start */
  inp = 576;
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR) 
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  happy = 1;
  outp = 0;
  osp = 0;
  while (happy)
    {
      if (inp >= totalin)
	{
	  if (totalin < PDP_BUF_SIZE) 
	    happy = 0;
	  else 
	    {
	      totalin = read(fd,buf,PDP_BUF_SIZE); 
	      inp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs,(unsigned char *)hdr,TRANS_BUF_SIZE) == MUS_ERROR) 
	    {
	      CLEANUP();
	      RETURN_MUS_WRITE_ERROR(oldname,newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  if (mode == 4)
	    {
	      /* packed 4 bits junk | 16 bit | 16 bit per each 36 */
	      /* so we grab four at a time here to keep the pointers aligned */
	      /* we've chosen an input buffer size that is a multiple of 9 so that this code need not constantly check bounds */
	      val = ((buf[inp] & 0xF) << 12) | (buf[inp+1] << 4) | (buf[inp+2] >> 4);
	      mus_bshort_to_char((unsigned char *)(hdr+osp),val); osp+=2;
	      val = ((buf[inp+2] & 0xF) << 12) | (buf[inp+3] << 4) | (buf[inp+4] >> 4);
	      mus_bshort_to_char((unsigned char *)(hdr+osp),val); osp+=2;
	      mus_bshort_to_char((unsigned char *)(hdr+osp),((buf[inp+5]<<8) | buf[inp+6])); osp+=2;
	      mus_bshort_to_char((unsigned char *)(hdr+osp),((buf[inp+7]<<8) | buf[inp+8])); osp+=2;
	      outp += 8;
	      inp += 9;
	    }
	  else
	    {
	      val = (buf[inp] << 8) | (buf[inp+1] & 0xF0);
	      mus_bshort_to_char((unsigned char *)(hdr+osp),val); osp+=2;
	      val = ((buf[inp+1] & 0xF) << 12) | (buf[inp+2] << 4);
	      mus_bshort_to_char((unsigned char *)(hdr+osp),val); osp+=2;
	      outp += 4;
	      inp += 3;
	    }
	}
    }
  err = snd_checked_write(fs,(unsigned char *)hdr,outp);
  CLEANUP();
  if (err == MUS_ERROR) RETURN_MUS_WRITE_ERROR(oldname,newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- IBM CVSD --------------------------------
 *
 * sox11 cvsd.c claims there's a spec for some form of this silliness:
 *      The CVSD format is described in the MIL Std 188 113, which is
 *      available from http://bbs.itsi.disa.mil:5580/T3564
 *
 * it also pushes the bits through a filter, and counts down from bit 7 to 0,
 * but it's definitely different from the CVSD as intended in a wav file.
 */

static int read_ibm_cvsd(char *oldname, char *newname, char *hdr)
{
  /* assumed to be in a RIFF file, and that we just read the header via c_read_header */
  /* avg rate gives srate/8 (8 bits per byte) -- can be ignored, can be stereo */
  int fs=-1,fd=-1,loc,totalin,happy,chans,srate,inp,outp,i,chn,byte,err=MUS_NO_ERROR;
  int *curvals;
  int osp;
  unsigned char *buf=NULL;
  STARTUP(oldname,newname,TRANS_BUF_SIZE,unsigned char);
  loc = mus_sound_data_location(oldname);
  chans = mus_sound_chans(oldname);
  curvals = (int *)CALLOC(chans,sizeof(int));
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((unsigned char *)(hdr+16),srate);
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR)
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  lseek(fd,loc,SEEK_SET);
  totalin = read(fd,buf,TRANS_BUF_SIZE);
  happy = 1;
  inp = 0;
  outp = 0;
  osp = 0;
  while (happy)
    {
      if (inp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = 0;
	  else 
	    {
	      totalin = read(fd,buf,TRANS_BUF_SIZE); 
	      inp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs,(unsigned char *)hdr,TRANS_BUF_SIZE) == MUS_ERROR) 
	    {
	      CLEANUP();
	      RETURN_MUS_WRITE_ERROR(oldname,newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  /* each byte becomes 8 samples */
	  chn = 0;
	  byte = buf[inp]; inp++;
	  for (i=0;i<8;i++)
	    {
	      /* are the bits consumed low to high or high to low? assume low to high for now (count i down from 7 to 0 if high to low) */
	      if (byte & (1<<i)) curvals[chn]++; else curvals[chn]--;
	      mus_bshort_to_char((unsigned char *)(hdr+osp),curvals[chn]); osp+=2; chn++;
	      if (chn == chans) chn = 0;
	    }
	  outp+=16;
	}
    }
  err = snd_checked_write(fs,(unsigned char *)hdr,outp);
  FREE(curvals);
  CLEANUP();
  if (err == MUS_ERROR) RETURN_MUS_WRITE_ERROR(oldname,newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- HCOM (from Sox) -------------------------------- */

static int read_hcom(char *oldname, char *newname, char *hdr)
{
  short **d;
  int osp,isp;
  int dc,di,bits,outp,happy,totalin;
  unsigned int curval = 0;
  int i,sample,size,datum,count,err=MUS_NO_ERROR;
  unsigned char *buf=NULL;
  int fd=-1,fs=-1;
  STARTUP(oldname,newname,TRANS_BUF_SIZE,unsigned char);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR)
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  lseek(fd,132,SEEK_SET);
  read(fd,buf,18);  /* count sum type div size */
  count = mus_char_to_bint((unsigned char *)buf) - 1;
  dc = mus_char_to_bint((unsigned char *)(buf+8));
  size = mus_char_to_bshort((unsigned char *)(buf+16));
  d = (short **)CALLOC(size,sizeof(short *));
  read(fd,buf,size*4+2); /* 2 for pad byte + first sample */
  osp = 0;
  for (i=0;i<size;i++) 
    {
      d[i] = (short *)CALLOC(2,sizeof(short));
      d[i][0] = mus_char_to_bshort((unsigned char *)(buf+osp)); osp+=2;
      d[i][1] = mus_char_to_bshort((unsigned char *)(buf+osp)); osp+=2;
    }
  sample = mus_char_to_bshort((unsigned char *)(buf+osp)) & 0xff;
  di = 0;
  totalin=read(fd,buf,TRANS_BUF_SIZE);
  osp = 0;
  isp = 0;
  happy = 1;
  outp = 2;
  mus_bshort_to_char((unsigned char *)(hdr+osp),(sample - 128) * 0x100); osp+=2;
  bits = 0;
  while ((happy) && (count>0))
    {
      if (isp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = 0;
	  else 
	    {
	      totalin = read(fd,buf,TRANS_BUF_SIZE); 
	      isp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs,(unsigned char *)hdr,TRANS_BUF_SIZE) == MUS_ERROR) 
	    {
	      for (i=0;i<size;i++) FREE(d[i]);
	      FREE(d);
	      CLEANUP();
	      RETURN_MUS_WRITE_ERROR(oldname,newname);
	    }
	  osp = 0;
	  outp = 0;
	}
      if (happy)
	{
	  if (bits == 0) 
	    {
	      curval = mus_char_to_bint((unsigned char *)(buf+isp)); 
	      isp+=4; 
	      bits = 32;
	    }
	  if (curval & 0x80000000) di = d[di][1]; else di = d[di][0];
	  curval = curval << 1;
	  bits--;
	  if(d[di][0] < 0) 
	    {
	      datum = d[di][1];
	      if (!dc) sample = 0;
	      sample = (sample + datum) & 0xff;
	      count--;
	      if (sample == 0) mus_bshort_to_char((unsigned char *)(hdr+osp),(-127 * 0x100));
	      else mus_bshort_to_char((unsigned char *)(hdr+osp),((sample - 128) * 0x100));
	      osp+=2;
	      outp+=2;
	      di = 0;
	    }
	}
    }
  err = snd_checked_write(fs,(unsigned char *)hdr,outp);
  for (i=0;i<size;i++) FREE(d[i]);
  FREE(d);
  CLEANUP();
  if (err == MUS_ERROR) RETURN_MUS_WRITE_ERROR(oldname,newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- NIST shortpack -------------------------------- */

static unsigned short log2s[] = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768};

static int read_nist_shortpack(char *oldname, char *newname, char *hdr)
{
  /* assume all relevant header stuff is ready via c_read_header */
  int fs=-1,fd=-1,err=MUS_NO_ERROR,totalin,happy,chans,srate,outp,i = 0,k,num,bits = 0,out,els = 0;
  int isp,osp;
  unsigned short *ptr = NULL,*stop,*start,*kptr;
  short temp = 0;
  unsigned char negative;
  unsigned char *buf=NULL;
  chans = mus_sound_chans(oldname);
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((unsigned char *)(hdr+16),srate);
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  STARTUP(oldname,newname,TRANS_BUF_SIZE,unsigned char);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR)
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  lseek(fd,1024,SEEK_SET);                       /* NIST header always 1024 bytes */
  totalin = read(fd,buf,TRANS_BUF_SIZE);
  happy = 1;
  outp = 0;
  num = 0;
  osp = 0; /* hdr */
  isp = 0; /* buf */
  start = &(log2s[15]);
  stop = log2s;
  while (happy)
    {
      /* now the shortpack algorithm, taken from wavio's shortpack_io.c */
      if (num == 0)
	{
	  num = (int)buf[isp]; 
	  bits = (int)buf[isp+1];
	  isp+=2; 
	  if (isp>=totalin) 
	    {
	      totalin = read(fd,buf,TRANS_BUF_SIZE); 
	      isp = 0;
	    }
	  temp = mus_char_to_bshort((unsigned char *)(buf+isp)); 
	  isp+=2;
	  if (isp>=totalin) 
	    {
	      totalin = read(fd,buf,TRANS_BUF_SIZE); 
	      isp = 0;
	    }
	  ptr = start;
	  i = 0;
	  els = (int)((num * (bits + 1)) / 16.0);
	  if ((num * (bits + 1)) % 16 != 0) els++;
	  els--;
	}
      else
	{
	  /* get next sample */
	  out = 0;
	  negative = ((temp & *(ptr--)) != 0);
	  if (ptr < stop)
	    {
	      ptr = start;
	      if (els > 0)
		{
		  temp = mus_char_to_bshort((unsigned char *)(buf+isp)); 
		  isp+=2;
		  if (isp>=totalin) 
		    {
		      totalin = read(fd,buf,TRANS_BUF_SIZE); 
		      isp = 0;
		    }
		  els--;
		}
	    }
	  kptr = &(log2s[bits - 1]);
	  for (k = bits + 1; (--k) > 0;)
	    {
	      if ((temp & *(ptr--)) != 0) out |= *kptr;
	      kptr--;
	      if (ptr < stop)
		{
		  ptr = start;
		  if (els > 0)
		    {
		      temp = mus_char_to_bshort((unsigned char *)(buf+isp)); 
		      isp+=2;
		      if (isp>=totalin) 
			{
			  totalin = read(fd,buf,TRANS_BUF_SIZE); 
			  isp = 0;
			}
		      els--;
		    }
		}
	    }
	  if (negative)
	    {
	      if (out != 0) 
		mus_bshort_to_char((unsigned char *)(hdr+osp),-out);
	      else mus_bshort_to_char((unsigned char *)(hdr+osp),32767);
	    }
	  else mus_bshort_to_char((unsigned char *)(hdr+osp),out);
	  osp+=2; 
	  outp+=2;
	  i++;
	  if (i == num) num=0;
	}
      if (isp >= totalin)
	{
	  if (totalin < TRANS_BUF_SIZE) 
	    happy = 0;
	  else 
	    {
	      totalin = read(fd,buf,TRANS_BUF_SIZE); 
	      isp = 0;
	    }
	}
      if (outp >= TRANS_BUF_SIZE) 
	{
	  if (snd_checked_write(fs,(unsigned char *)hdr,TRANS_BUF_SIZE) == MUS_ERROR)
	    {
	      CLEANUP();
	      RETURN_MUS_WRITE_ERROR(oldname,newname);
	    }
	  osp = 0;
	  outp = 0;
	}
    }
  err = snd_checked_write(fs,(unsigned char *)hdr,outp);
  CLEANUP();
  if (err == MUS_ERROR) RETURN_MUS_WRITE_ERROR(oldname,newname);
  return(MUS_NO_ERROR);
}


/* -------------------------------- Intel ADPCM --------------------------------
 *
 * described in detail Microsoft RIFF docs.  This code assumes bits=4.
 * in 'wave' file, these are stored as block_align sized blocks, each with a
 * header storing the current state.  These can be multi-channel, but we're handling
 * only mono until someone complains.  See also Apple Tech note 1081 by Mark Cookson.
 */

static int indexTable[16] = {-1, -1, -1, -1, 2, 4, 6, 8, -1, -1, -1, -1, 2, 4, 6, 8};
static int stepsizeTable[89] = {7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41, 45,
				50, 55, 60, 66, 73, 80, 88, 97, 107, 118, 130, 143, 157, 173, 190, 209, 230, 253, 279, 307,
				337, 371, 408, 449, 494, 544, 598, 658, 724, 796, 876, 963, 1060, 1166, 1282, 1411, 1552, 1707, 1878, 2066,
				2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358,
				5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899,
				15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767};

static int adpcm_decoder(unsigned char *indata, short *outdata, int totalbytes, int type)
{
  unsigned int delta,inputbuffer = 0;
  int step,valpred,vpdiff,index,bufferstep,i,j,happy;
  bufferstep = 0;
  happy = 1;
  if (type == 0)
    {
      j = 4;
      valpred = mus_char_to_lshort(indata);
      index = indata[2];
    }
  else
    {
      j = 2;
      index = indata[1] & 0x7f;
      valpred = (indata[0]*0x100) + (indata[1]&0xff80);
    }
  i=1;
  outdata[0] = valpred;
  while (happy)
    {
      if (bufferstep) 
	{
	  delta = inputbuffer & 0xf;
	  if (j == totalbytes) happy=0;
	} 
      else 
	{
	  inputbuffer = indata[j++];
	  delta = (inputbuffer>>4) & 0xf;
	}
      bufferstep = !bufferstep;
      step = stepsizeTable[index];
      vpdiff = (step>>3);
      if (delta & 1) vpdiff += (step>>2);
      if (delta & 2) vpdiff += (step>>1);
      if (delta & 4) vpdiff += step;
      if (delta & 8) valpred -= vpdiff; else valpred += vpdiff;
      if (valpred > 32767)  valpred = 32767; else if (valpred < -32768)  valpred = -32768;
      outdata[i++] = valpred;
      index += indexTable[delta];
      if (index < 0) index = 0; else if (index > 88) index = 88;
    }
  return(i);
}


static int read_dvi_adpcm(char *oldname, char *newname, char *hdr, int type)
{
  int fs=-1,fd=-1,loc,totalin,chans,srate,blksiz,samps,samps_read;
  unsigned char *buf=NULL;
  loc = mus_sound_data_location(oldname);
  chans = mus_sound_chans(oldname);
  blksiz = mus_sound_align(oldname);
  samps = mus_sound_fact_samples(oldname);
  if ((chans != 1) || (mus_sound_bits_per_sample(oldname) != 4))
    {
      mus_error(MUS_UNSUPPORTED_DATA_FORMAT,
		"can't translate DVI ADPCM file %s: chans: %d and bits: %d\n  [snd-trans.c[%d] %s]",
		oldname,chans,mus_sound_bits_per_sample(oldname),
		__LINE__,__FUNCTION__);
      return(MUS_ERROR);
    }
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((unsigned char *)(hdr+16),srate);
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  STARTUP(oldname,newname,blksiz,unsigned char);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR)
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  lseek(fd,loc,SEEK_SET);
  samps_read = 0;
  while (samps > 0)
    {
      totalin = read(fd,buf,blksiz);
      if (totalin < blksiz) break;
      samps_read = adpcm_decoder(buf,(short *)hdr,totalin,type);
      if (be_snd_checked_write(fs,(unsigned char *)hdr,samps_read*2) == MUS_ERROR) 
	{
	  CLEANUP();
	  RETURN_MUS_WRITE_ERROR(oldname,newname);
	}
      samps -= samps_read;
    }
  CLEANUP();
  return(MUS_NO_ERROR);
}



/* --------------------------------Oki (Dialogic) ADPCM --------------------------------
 *
 * from vox.tar.gz:
 *   "PC Telephony - The complete guide to designing, building and programming systems
 *    using Dialogic and Related Hardware" by Bob Edgar. pg 272-276.
 */

struct oki_adpcm_status {short last; short step_index;};

static short oki_step_size[49] = { 16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41,
     45, 50, 55, 60, 66, 73, 80, 88, 97, 107, 118, 130, 143, 157, 173,
     190, 209, 230, 253, 279, 307, 337, 371, 408, 449, 494, 544, 598, 658,
     724, 796, 876, 963, 1060, 1166, 1282, 1408, 1552 };

static short oki_adjust[8]={-1,-1,-1,-1,2,4,6,8};

static short oki_adpcm_decode(char code, struct oki_adpcm_status *stat) 
{
  short diff,E,SS,samp;
  SS = oki_step_size[stat->step_index];
  E = SS/8;
  if (code & 0x01) E += SS/4;
  if (code & 0x02) E += SS/2;
  if (code & 0x04) E += SS;
  diff = (code & 0x08) ? -E : E;
  samp = stat->last + diff;
  if (samp > 2048) samp = 2048;
  if (samp < -2048) samp = -2048;
  stat->last = samp;
  stat->step_index += oki_adjust[code & 0x07];
  if (stat->step_index < 0) stat->step_index = 0;
  if (stat->step_index > 48) stat->step_index = 48;
  return(samp<<4);
}

static int read_oki_adpcm(char *oldname, char *newname, char *hdr)
{
  int fs=-1,fd=-1,loc,i,j,totalin,chans,srate,blksiz,samps,samps_read;
  unsigned char *buf=NULL;
  short *buf1;
  struct oki_adpcm_status stat;
  chans = mus_sound_chans(oldname);
  if (chans != 1)
    {
      mus_error(MUS_UNSUPPORTED_DATA_FORMAT,
		"can't translate Oki ADPCM file %s: chans: %d\n  [snd-trans.c[%d] %s]",
		oldname,chans,
		__LINE__,__FUNCTION__);
      return(MUS_ERROR);
    }
  loc = mus_sound_data_location(oldname);
  blksiz = mus_sound_align(oldname);
  if (blksiz == 0) blksiz=256;
  STARTUP(oldname,newname,blksiz,unsigned char);
  buf1 = (short *)CALLOC(blksiz*2,sizeof(short));
  samps = mus_sound_fact_samples(oldname);
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((unsigned char *)(hdr+16),srate);
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  fd = open(oldname,O_RDONLY,0);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR) 
    {
      FREE(buf1);
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  lseek(fd,loc,SEEK_SET);
  samps_read = 0;
  stat.last = 0;
  stat.step_index = 0;
  while (samps > 0)
    {
      totalin = read(fd,buf,blksiz);
      if (totalin <= 0) break;
      for (i=0,j=0;i<totalin;i++)
	{
	  /* samps_read will be twice totalin because these are 4-bit quantities */
	  buf1[j++] = oki_adpcm_decode((char)((buf[i]>>4) & 0x0f),&stat);
	  buf1[j++] = oki_adpcm_decode((char)(buf[i]&0x0f),&stat);
	}
      samps_read = totalin*2;
      if (be_snd_checked_write(fs,(unsigned char *)buf1,samps_read*2) == MUS_ERROR) 
	{
	  FREE(buf1);
	  CLEANUP();
	  RETURN_MUS_WRITE_ERROR(oldname,newname);
	}
      samps -= samps_read;
    }
  FREE(buf1);
  CLEANUP();
  return(MUS_NO_ERROR);
}

/* -------------------------------- 12 bit cases --------------------------------
 */

static int read_12bit(char *oldname, char *newname, char *hdr)
{
  int loc,chans,samps,totalin,i,j,fs=-1,fd=-1;
  unsigned char *buf=NULL;
  short *buf1;
  loc = mus_sound_data_location(oldname);
  chans = mus_sound_chans(oldname);
  samps = mus_sound_samples(oldname);
  mus_bint_to_char((unsigned char *)(hdr+16),mus_sound_srate(oldname));
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  STARTUP(oldname,newname,((int)(TRANS_BUF_SIZE*1.5)),unsigned char);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR)
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  lseek(fd,loc,SEEK_SET);
  buf1 = (short *)CALLOC(TRANS_BUF_SIZE,sizeof(short));
  while (samps > 0)
    {
      totalin = read(fd,buf,(int)(TRANS_BUF_SIZE*1.5));
      if (totalin <= 0) break;
      for (i=0,j=0;i<totalin;i+=3,j+=2)
	{
	  buf1[j] = (signed short)((buf[i]<<8) + (buf[i+1]&0xf0));
	  buf1[j+1] = (signed short)((buf[i+2]<<8) + ((buf[i+1]&0xf)<<4));
	}
      if (be_snd_checked_write(fs,(unsigned char *)buf1,j*2) == MUS_ERROR) 
	{
	  FREE(buf1);
	  CLEANUP();
	  RETURN_MUS_WRITE_ERROR(oldname,newname);
	}
      samps -= j;
    }
  FREE(buf1);
  CLEANUP();
  return(MUS_NO_ERROR);
}


/* -------------------------------- IFF Fibonacci and Exponential --------------------------------
 */

static int fb[] = {-34,-21,-13,-8,-5,-3,-2,-1,0,1,2,3,5,8,13,21};
static int ex[] = {-128,-64,-32,-16,-8,-4,-2,-1,0,1,2,4,8,16,32,64};

static int read_iff(char *oldname, char *newname, int orig, char *hdr)
{
  int loc,chans,samps,totalin,i,j,fs=-1,fd=-1,f1,f2,val;
  short *buf=NULL;
  loc = mus_sound_data_location(oldname);
  chans = mus_sound_chans(oldname);
  samps = mus_sound_samples(oldname);
  mus_bint_to_char((unsigned char *)(hdr+16),mus_sound_srate(oldname));
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  STARTUP(oldname,newname,TRANS_BUF_SIZE*2,short);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR)
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  lseek(fd,loc,SEEK_SET);
  val = 0;
  while (samps > 0)
    {
      totalin = read(fd,hdr,TRANS_BUF_SIZE);
      if (totalin <= 0) break;
      for (i=0,j=0;i<totalin;i++,j+=2)
	{
	  f1 = ((unsigned char)hdr[i])&0xf;
	  f2 = (((unsigned char)hdr[i])>>4)&0xf;
	  if (orig == 1)
	    {
	      val += fb[f1]; /* might want a shift << 8 here or something */
	      buf[j] = val;
	      val += fb[f2];
	      buf[j+1] = val;
	    }
	  else
	    {
	      val += ex[f1];
	      buf[j] = val;
	      val += ex[f2];
	      buf[j+1] = val;
	    }
	}
      if (be_snd_checked_write(fs,(unsigned char *)buf,j*2) == MUS_ERROR)
	{
	  CLEANUP();
	  RETURN_MUS_WRITE_ERROR(oldname,newname);
	}
      samps -= j;
    }
  CLEANUP();
  return(MUS_NO_ERROR);
}


/*  -------------------------------- AVI --------------------------------
 *
 * data is squirreled away in ##wb data blocks somewhere within a LIST chunk
 * we have to mimic the header reader to find these guys one by one.
 * we assume we've got 16-bit linear little endian data here all in a single sequence of 'wb' blocks.
 * (in the 'rec' case, audio and video data can be interleaved, but that's too bad)
 */

static int read_avi(char *oldname, char *newname, char *hdr)
{
  int totalin,fs=-1,fd=-1,cksize,num,happy;
#ifndef MUS_LITTLE_ENDIAN
  int i;
  unsigned char *bb;
#endif
  short *buf=NULL;
  unsigned char *hdrbuf;
  mus_bint_to_char((unsigned char *)(hdr+16),mus_sound_srate(oldname));
  mus_bint_to_char((unsigned char *)(hdr+20),mus_sound_chans(oldname));
  STARTUP(oldname,newname,TRANS_BUF_SIZE,short);
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR)
    {
      CLEANUP();
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  hdrbuf = (unsigned char *)CALLOC(8,sizeof(unsigned char));
  lseek(fd,mus_sound_data_location(oldname),SEEK_SET);
  happy = 1;
  while (happy)
    {
      totalin = read(fd,hdrbuf,8);
      if (totalin < 0) break;
      cksize = mus_char_to_lint((unsigned char *)(hdrbuf+4));
      if ((hdrbuf[2] == 'w') && (hdrbuf[3] == 'b'))
	{
	  while (cksize > 0)
	    {
	      if (TRANS_BUF_SIZE*2 > cksize)
		num = cksize;
	      else num = TRANS_BUF_SIZE*2;
	      totalin = read(fd,(unsigned char *)buf,num);
	      if (totalin < 0) 
		{
		  happy = 0; 
		  break;
		}
	      else 
		{
#ifndef MUS_LITTLE_ENDIAN
		  bb = (unsigned char *)buf;
		  for (i=0;i<totalin/2;i++,bb+=2) buf[i] = mus_char_to_lshort(bb);
#endif		  
		  if (be_snd_checked_write(fs,(unsigned char *)buf,totalin) == MUS_ERROR) 
		    {
		      FREE(hdrbuf);
		      CLEANUP();
		      RETURN_MUS_WRITE_ERROR(oldname,newname);
		    }
		}
	      cksize -= num;
	    }
	}
      else break;
    }
  FREE(hdrbuf);
  CLEANUP();
  return(MUS_NO_ERROR);
}


/*  -------------------------------- G721 and G723 from Sun --------------------------------
 * code boiled down considerably here since I have no love of compression schemes.
 */

struct g72x_state {long yl; short yu; short dms; short dml; short ap; short a[2]; short b[6]; short pk[2]; short dq[6]; short sr[2]; char td;};
static short power2[15] = {1, 2, 4, 8, 0x10, 0x20, 0x40, 0x80, 0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000};

static int quan(int val,short *table,int size)
{
  int i;
  for (i=0;i<size;i++)if (val < *table++) break;
  return (i);
}

static int fmult(int an,int srn)
{
  short	anmag,anexp,anmant;
  short	wanexp,wanmant;
  short	retval;
  anmag = (an > 0) ? an : ((-an) & 0x1FFF);
  anexp = quan(anmag, power2, 15) - 6;
  anmant = (anmag == 0) ? 32 : (anexp >= 0) ? anmag >> anexp : anmag << -anexp;
  wanexp = anexp + ((srn >> 6) & 0xF) - 13;
  wanmant = (anmant * (srn & 077) + 0x30) >> 4;
  retval = (wanexp >= 0) ? ((wanmant << wanexp) & 0x7FFF) : (wanmant >> -wanexp);
  return (((an ^ srn) < 0) ? -retval : retval);
}

static void g72x_init_state(struct g72x_state *state_ptr)
{
  int cnta;
  state_ptr->yl = 34816;
  state_ptr->yu = 544;
  state_ptr->dms = 0;
  state_ptr->dml = 0;
  state_ptr->ap = 0;
  for (cnta=0;cnta<2;cnta++)
    {
      state_ptr->a[cnta] = 0;
      state_ptr->pk[cnta] = 0;
      state_ptr->sr[cnta] = 32;
    }
  for (cnta=0;cnta<6;cnta++) 
    {
      state_ptr->b[cnta] = 0;
      state_ptr->dq[cnta] = 32;
    }
  state_ptr->td = 0;
}

static int predictor_zero(struct g72x_state *state_ptr)
{
  int i,sezi;
  sezi = fmult(state_ptr->b[0] >> 2, state_ptr->dq[0]);
  for (i=1;i<6;i++) sezi += fmult(state_ptr->b[i] >> 2, state_ptr->dq[i]);
  return (sezi);
}

static int predictor_pole(struct g72x_state *state_ptr)
{
  return (fmult(state_ptr->a[1] >> 2, state_ptr->sr[1]) + fmult(state_ptr->a[0] >> 2, state_ptr->sr[0]));
}

static int step_size(struct g72x_state *state_ptr)
{
  int y,dif,al;
  if (state_ptr->ap >= 256)  return (state_ptr->yu);
  else 
    {
      y = state_ptr->yl >> 6;
      dif = state_ptr->yu - y;
      al = state_ptr->ap >> 2;
      if (dif > 0) y += (dif * al) >> 6;
      else if (dif < 0) y += (dif * al + 0x3F) >> 6;
      return (y);
    }
}

static int reconstruct(int sign,int dqln,int y)
{
  short	dql,dex,dqt,dq;
  dql = dqln + (y >> 2);
  if (dql < 0) {return ((sign) ? -0x8000 : 0);} 
  else {
    dex = (dql >> 7) & 15;
    dqt = 128 + (dql & 127);
    dq = (dqt << 7) >> (14 - dex);
    return ((sign) ? (dq - 0x8000) : dq);
  }
}

static void update(int	code_size,int y,int wi,int fi,int dq,int sr,int dqsez,struct g72x_state *state_ptr)
{
  int cnt;
  short	mag,exp,a2p=0,a1ul,pks1,fa1,ylint,thr2,dqthr,ylfrac,thr1,pk0;
  char tr;
  pk0 = (dqsez < 0) ? 1 : 0;
  mag = dq & 0x7FFF;
  ylint = state_ptr->yl >> 15;
  ylfrac = (state_ptr->yl >> 10) & 0x1F;
  thr1 = (32 + ylfrac) << ylint;
  thr2 = (ylint > 9) ? 31 << 10 : thr1;
  dqthr = (thr2 + (thr2 >> 1)) >> 1; 
  if (state_ptr->td == 0) tr = 0; else if (mag <= dqthr) tr = 0; else tr = 1;
  state_ptr->yu = y + ((wi - y) >> 5);
  if (state_ptr->yu < 544) state_ptr->yu = 544; else if (state_ptr->yu > 5120) state_ptr->yu = 5120;
  state_ptr->yl += state_ptr->yu + ((-state_ptr->yl) >> 6);
  if (tr == 1) {
    state_ptr->a[0] = 0;
    state_ptr->a[1] = 0;
    state_ptr->b[0] = 0;
    state_ptr->b[1] = 0;
    state_ptr->b[2] = 0;
    state_ptr->b[3] = 0;
    state_ptr->b[4] = 0;
    state_ptr->b[5] = 0;
  } else { 
    pks1 = pk0 ^ state_ptr->pk[0];		/* UPA2 */
    a2p = state_ptr->a[1] - (state_ptr->a[1] >> 7);
    if (dqsez != 0) {
      fa1 = (pks1) ? state_ptr->a[0] : -state_ptr->a[0];
      if (fa1 < -8191) a2p -= 0x100; else if (fa1 > 8191) a2p += 0xFF; else a2p += fa1 >> 5;
      if (pk0 ^ state_ptr->pk[1])
	{if (a2p <= -12160) a2p = -12288; else {if (a2p >= 12416) a2p = 12288; else a2p -= 0x80;}}
      else {if (a2p <= -12416) a2p = -12288; else {if (a2p >= 12160) a2p = 12288; else a2p += 0x80;}}
    }
    state_ptr->a[1] = a2p;
    state_ptr->a[0] -= state_ptr->a[0] >> 8;
    if (dqsez != 0) {if (pks1 == 0) state_ptr->a[0] += 192; else state_ptr->a[0] -= 192;}
    a1ul = 15360 - a2p;
    if (state_ptr->a[0] < -a1ul) state_ptr->a[0] = -a1ul; else if (state_ptr->a[0] > a1ul) state_ptr->a[0] = a1ul;
    for (cnt=0;cnt<6;cnt++) 
      {
	if (code_size == 5) state_ptr->b[cnt] -= state_ptr->b[cnt] >> 9;
	else state_ptr->b[cnt] -= state_ptr->b[cnt] >> 8;
	if (dq & 0x7FFF) 
	  {
	    if ((dq ^ state_ptr->dq[cnt]) >= 0) state_ptr->b[cnt] += 128;
	    else state_ptr->b[cnt] -= 128;
	  }
      }
  }
  for (cnt=5;cnt>0;cnt--) state_ptr->dq[cnt] = state_ptr->dq[cnt-1];
  if (mag == 0) 
    {
      state_ptr->dq[0] = (dq >= 0) ? 0x20 : 0xFC20;
    } 
  else 
    {
      exp = quan(mag, power2, 15);
      state_ptr->dq[0] = (dq >= 0) ?
	(exp << 6) + ((mag << 6) >> exp) :
	(exp << 6) + ((mag << 6) >> exp) - 0x400;
    }
  state_ptr->sr[1] = state_ptr->sr[0];
  if (sr == 0) 
    {
      state_ptr->sr[0] = 0x20;
    } 
  else 
    if (sr > 0) 
      {
	exp = quan(sr, power2, 15);
	state_ptr->sr[0] = (exp << 6) + ((sr << 6) >> exp);
      } 
    else 
      if (sr > -32768) 
	{
	  mag = -sr;
	  exp = quan(mag, power2, 15);
	  state_ptr->sr[0] =  (exp << 6) + ((mag << 6) >> exp) - 0x400;
	} 
      else
	state_ptr->sr[0] = 0xFC20;
  state_ptr->pk[1] = state_ptr->pk[0];
  state_ptr->pk[0] = pk0;
  if (tr == 1) state_ptr->td = 0; else if (a2p < -11776) state_ptr->td = 1; else state_ptr->td = 0;
  state_ptr->dms += (fi - state_ptr->dms) >> 5;
  state_ptr->dml += (((fi << 2) - state_ptr->dml) >> 7);
  if (tr == 1)
    state_ptr->ap = 256;
  else if (y < 1536)
    state_ptr->ap += (0x200 - state_ptr->ap) >> 4;
  else if (state_ptr->td == 1)
    state_ptr->ap += (0x200 - state_ptr->ap) >> 4;
  else if (abs((state_ptr->dms << 2) - state_ptr->dml) >= (state_ptr->dml >> 3))
    state_ptr->ap += (0x200 - state_ptr->ap) >> 4;
  else
    state_ptr->ap += (-state_ptr->ap) >> 4;
}

static int g721_decoder(int i,struct g72x_state *state_ptr)
{
  static short dqlntab[16] = {-2048, 4, 135, 213, 273, 323, 373, 425, 425, 373, 323, 273, 213, 135, 4, -2048};
  static short witab[16] = {-12, 18, 41, 64, 112, 198, 355, 1122, 1122, 355, 198, 112, 64, 41, 18, -12};
  static short fitab[16] = {0, 0, 0, 0x200, 0x200, 0x200, 0x600, 0xE00, 0xE00, 0x600, 0x200, 0x200, 0x200, 0, 0, 0};
  short	sezi,sei,sez,se,y,sr,dq,dqsez;
  i &= 0x0f;
  sezi = predictor_zero(state_ptr);
  sez = sezi >> 1;
  sei = sezi + predictor_pole(state_ptr);
  se = sei >> 1;
  y = step_size(state_ptr);
  dq = reconstruct(i & 0x08, dqlntab[i], y);
  sr = (dq < 0) ? (se - (dq & 0x3FFF)) : se + dq;
  dqsez = sr - se + sez;
  update(4, y, witab[i] << 5, fitab[i], dq, sr, dqsez, state_ptr);
  return (sr << 2);
}

static int g723_24_decoder(int	i,struct g72x_state *state_ptr)
{
  static short dqlntab[8] = {-2048, 135, 273, 373, 373, 273, 135, -2048};
  static short witab[8] = {-128, 960, 4384, 18624, 18624, 4384, 960, -128};
  static short fitab[8] = {0, 0x200, 0x400, 0xE00, 0xE00, 0x400, 0x200, 0};
  short	sezi,sei,sez,se,y,sr,dq,dqsez;
  i &= 0x07;
  sezi = predictor_zero(state_ptr);
  sez = sezi >> 1;
  sei = sezi + predictor_pole(state_ptr);
  se = sei >> 1;
  y = step_size(state_ptr);
  dq = reconstruct(i & 0x04, dqlntab[i], y); 
  sr = (dq < 0) ? (se - (dq & 0x3FFF)) : (se + dq);
  dqsez = sr - se + sez;
  update(3, y, witab[i], fitab[i], dq, sr, dqsez, state_ptr);
  return (sr << 2);
}

static int g723_40_decoder(int i,struct g72x_state *state_ptr)
{
  static short dqlntab[32] = {-2048, -66, 28, 104, 169, 224, 274, 318, 358, 395, 429, 459, 488, 514, 539, 566,
				566, 539, 514, 488, 459, 429, 395, 358, 318, 274, 224, 169, 104, 28, -66, -2048};
  static short witab[32] = {448, 448, 768, 1248, 1280, 1312, 1856, 3200, 4512, 5728, 7008, 8960, 11456, 14080, 16928, 22272,
			      22272, 16928, 14080, 11456, 8960, 7008, 5728, 4512, 3200, 1856, 1312, 1280, 1248, 768, 448, 448};
  static short fitab[32] = {0, 0, 0, 0, 0, 0x200, 0x200, 0x200, 0x200, 0x200, 0x400, 0x600, 0x800, 0xA00, 0xC00, 0xC00,
			      0xC00, 0xC00, 0xA00, 0x800, 0x600, 0x400, 0x200, 0x200, 0x200, 0x200, 0x200, 0, 0, 0, 0, 0};
  short	sezi,sei,se,sez,y,sr,dq,dqsez;
  i &= 0x1f;
  sezi = predictor_zero(state_ptr);
  sez = sezi >> 1;
  sei = sezi + predictor_pole(state_ptr);
  se = sei >> 1;
  y = step_size(state_ptr);
  dq = reconstruct(i & 0x10, dqlntab[i], y);
  sr = (dq < 0) ? (se - (dq & 0x7FFF)) : (se + dq);
  dqsez = sr - se + sez;
  update(5, y, witab[i], fitab[i], dq, sr, dqsez, state_ptr);
  return (sr << 2);
}

static int unpack_input(FILE *fin, unsigned char *code,int bits)
{
  static unsigned int in_buffer = 0;
  static int in_bits = 0;
  unsigned char	in_byte;
  if (in_bits < bits) 
    {
      if (fread(&in_byte,sizeof(char),1,fin) != 1) 
	{
	  *code = 0;
	  return (-1);
	}
      in_buffer |= (in_byte << in_bits);
      in_bits += 8;
    }
  *code = in_buffer & ((1 << bits) - 1);
  in_buffer >>= bits;
  in_bits -= bits;
  return (in_bits > 0);
}

static int read_g72x_adpcm(char *oldname, char *newname, char *hdr, int which_g)
{
  int fs=-1,loc,j,chans,srate,dec_bits=0,err=MUS_NO_ERROR;
  FILE *fd;
  unsigned char code;
  short *buf=NULL;
  struct g72x_state state;
  g72x_init_state(&state);
  chans = mus_sound_chans(oldname);
  if (chans != 1)
    {
      mus_error(MUS_UNSUPPORTED_DATA_FORMAT,
		"can't translate G72x file %s: chans: %d\n  [snd-trans.c[%d] %s]",
		oldname,chans,
		__LINE__,__FUNCTION__);
      return(MUS_ERROR);
    }
  fs = creat(newname,0666);
  if (fs == -1) RETURN_MUS_IO_ERROR("create",newname);
  loc = mus_sound_data_location(oldname);
  srate = mus_sound_srate(oldname);
  mus_bint_to_char((unsigned char *)(hdr+16),srate);
  mus_bint_to_char((unsigned char *)(hdr+20),chans);
  fd = fopen(oldname,"rb");
  if (fd == NULL) 
    {
      close(fs); 
      RETURN_MUS_IO_ERROR("fopen",oldname);
    }
  if (snd_checked_write(fs,(unsigned char *)hdr,28) == MUS_ERROR) 
    {
      close(fs); 
      fclose(fd);
      RETURN_MUS_WRITE_ERROR(oldname,newname);
    }
  buf = (short *)CALLOC(TRANS_BUF_SIZE,sizeof(short));
  if (buf == NULL) 
    {
      close(fs); 
      fclose(fd); 
      RETURN_MUS_ALLOC_ERROR(oldname,TRANS_BUF_SIZE,"buf");
    }
  fread(buf,1,loc,fd);
  switch (which_g)
    {
    case 0: /* G721 */ dec_bits = 4; break;
    case 1: /* G723_24 */ dec_bits = 3; break;
    case 2: /* G723_40 */ dec_bits = 5; break;
    }
  j = 0;
  while (unpack_input(fd,&code,dec_bits) >= 0)
    {
      switch (which_g)
	{
	case 0: buf[j++] = g721_decoder(code,&state); break;
	case 1: buf[j++] = g723_24_decoder(code,&state); break;
	case 2: buf[j++] = g723_40_decoder(code,&state); break;
	}
      if (j >= TRANS_BUF_SIZE)
	{
	  if (be_snd_checked_write(fs,(unsigned char *)buf,j*2) == MUS_ERROR) 
	    {
	      close(fs); 
	      fclose(fd); 
	      FREE(buf); 
	      RETURN_MUS_WRITE_ERROR(oldname,newname);
	    }
	  j = 0;
	}
    }
  if (j>0) err = be_snd_checked_write(fs,(unsigned char *)buf,j*2);
  close(fs);
  fclose(fd);
  FREE(buf);
  if (err == MUS_ERROR) RETURN_MUS_WRITE_ERROR(oldname,newname);
  return(MUS_NO_ERROR);
}



/* -------------------------------- TRANSLATE -------------------------------- */

#define RIFF_IBM_CVSD 5
#define RIFF_Intel_ADPCM 0x11
#define RIFF_Oki_ADPCM 0x10
#define RIFF_G721 0x40
#define RIFF_G723 0x14
#define RIFF_MS_G723 0x42
#define RIFF_Lucent_G723 0x59
#define RIFF_Vivo_G723 0x111
#define RIFF_Gsm610 0x31
#define RIFF_G721 0x40
#define RIFF_MPEG 0x50
#define RIFF_MS_ADPCM 2
#define NeXT_G721 23
#define NeXT_G722 24
#define NeXT_G723 25
#define NeXT_G723_5 26

static int MUS_CANT_TRANSLATE = 0;

static char *any_format_name(char *name)
{
  int format;
  format = mus_sound_data_format(name);
  if (format != MUS_UNSUPPORTED)
    return(mus_data_format_name(format));
  else return(mus_header_original_format_name(mus_sound_original_format(name),
					      mus_sound_header_type(name)));
}

int snd_translate(char *oldname, char *newname, int type)
{
  /* read oldname, translate to newname as 16-bit linear NeXT file */
  /* called from snd-file.c */
  int orig,err;
  char *hdr = NULL;
  if (MUS_CANT_TRANSLATE == 0) MUS_CANT_TRANSLATE = mus_make_error("mus_cant_translate");
  err = MUS_CANT_TRANSLATE;
  hdr = (char *)CALLOC(TRANS_BUF_SIZE,sizeof(char));
  /* set up default output header */
  mus_bint_to_char((unsigned char *)hdr,0x2e736e64); /* .snd */
  mus_bint_to_char((unsigned char *)(hdr+4),28);     /* data location */
  mus_bint_to_char((unsigned char *)(hdr+8),0);      /* bytes in data portion */
  mus_bint_to_char((unsigned char *)(hdr+12),3);     /* 16-bit linear */
  mus_bint_to_char((unsigned char *)(hdr+16),22050);
  mus_bint_to_char((unsigned char *)(hdr+20),1);     /* chans */
  switch (type)
    {
    case MUS_MIDI_SAMPLE_DUMP: err = read_midi_sample_dump(oldname,newname,hdr); break;
    case MUS_IEEE: err = read_ieee_text(oldname,newname,hdr); break;
    case MUS_MUS10: err = read_mus10(oldname,newname,hdr); break;
    case MUS_HCOM: err = read_hcom(oldname,newname,hdr); break;
    case MUS_YAMAHA_TX16: err = read_12bit(oldname,newname,hdr); break;
    case MUS_AVI: err = read_avi(oldname,newname,hdr); break;
    case MUS_RIFF:
      switch (mus_sound_original_format(oldname))
	{
	case RIFF_IBM_CVSD: err = read_ibm_cvsd(oldname,newname,hdr); break;
	case RIFF_MS_ADPCM: case RIFF_Intel_ADPCM: err = read_dvi_adpcm(oldname,newname,hdr,0); break;
	case RIFF_Oki_ADPCM: err = read_oki_adpcm(oldname,newname,hdr); break;
	case RIFF_G721: err = read_g72x_adpcm(oldname,newname,hdr,0); break; /* untested */
	case RIFF_G723: case RIFF_MS_G723: case RIFF_Lucent_G723: case RIFF_Vivo_G723: /* untested */
	  if (mus_sound_bits_per_sample(oldname) == 3)
	    err = read_g72x_adpcm(oldname,newname,hdr,1);
	  else
	    if (mus_sound_bits_per_sample(oldname) == 5)
	      err = read_g72x_adpcm(oldname,newname,hdr,2);
	  break;
	}
      break;
    case MUS_NIST:
      if (mus_sound_original_format(oldname) == MUS_NIST_SHORTPACK) 
	err = read_nist_shortpack(oldname,newname,hdr); 
      break;
    case MUS_SVX:
      orig = mus_sound_original_format(oldname);
      if ((orig == 1) || (orig == 2)) 
	err = read_iff(oldname,newname,orig,hdr);
      break;
    case MUS_NEXT:
      switch (mus_sound_original_format(oldname))
	{
	case NeXT_G721: err = read_g72x_adpcm(oldname,newname,hdr,0); break;
	case NeXT_G723: err = read_g72x_adpcm(oldname,newname,hdr,1); break;
	case NeXT_G723_5: err = read_g72x_adpcm(oldname,newname,hdr,2); break;
	}
      break;
    case MUS_AIFC:
      if (mus_sound_original_format(oldname) == MUS_AIFF_IMA_ADPCM) 
	err = read_dvi_adpcm(oldname,newname,hdr,1); 
      break;
    case MUS_MATLAB:
      /* assume all vectors are channels */
      break;
    }
  FREE(hdr);
  if (err == MUS_CANT_TRANSLATE) /* i.e a case we don't even try to handle */
    mus_error(MUS_CANT_TRANSLATE,
	      "can't translate %s\n  (%s header: %s (0x%x) data format)\n  [snd-trans.c[%d] %s]",
	      oldname,
	      mus_header_type_name(type),
	      any_format_name(oldname),
	      mus_sound_original_format(oldname),
	      __LINE__,__FUNCTION__);
  return(err);
}




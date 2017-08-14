/*---------------------------------------------------------------*/
/* RTVDDSSRC - Retrieve DDS Source                               */
/*                                                               */
/* Michael Sansoterra, 01/20/03                                  */
/*                                                               */
/*                                                               */
/* Compile to call program: RTVDDSSRCC                           */
/*                                                               */
/* CRTCMD CMD(XXX/RTVDDSSRC) PGM(*LIBL/RTVDDSSRCC)               */
/*        SRCFILE(XXX/QCMDSRC)                                   */
/*                                                               */
/*---------------------------------------------------------------*/
             CMD        PROMPT('Retrieve DDS Source')

/* File Name Prompt */
             PARM       KWD(FILE) TYPE(FILEPMT) PROMPT('File Name')

/* Source File Name Prompt */
             PARM       KWD(SRCFILE) TYPE(SRCFILE) PROMPT('Source File +
                          Name')

/* Member Name */
             PARM       KWD(SRCMBR) TYPE(*NAME) LEN(10) DFT(*FILE) +
                          SPCVAL((*FILE)) PROMPT('Source Member')

/* Replace Existing Member */
             PARM       KWD(REPLACE) TYPE(*CHAR) LEN(4) RSTD(*YES) +
                          DFT(*YES) VALUES(*YES *NO) PROMPT('Replace +
                          existing member')


 FILEPMT:    QUAL       TYPE(*NAME) LEN(10)
             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL((*LIBL) +
                          (*CURLIB)) PROMPT('Library')

 SRCFILE:    QUAL       TYPE(*NAME) LEN(10) DFT(QDDSSRC) SPCVAL((QDDSSRC))
             QUAL       TYPE(*NAME) LEN(10) DFT(*CURLIB) SPCVAL((*LIBL) +
                          (*CURLIB)) PROMPT('Library')

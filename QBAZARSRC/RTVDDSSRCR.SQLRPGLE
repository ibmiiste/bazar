      *================================================================
      *
      *  RTVDDSSRCR  Build DDS Spec from file
      *              ILE Version
      *
      *
      *  Michael Sansoterra, 01/20/03
      *
      * Notes:
      * -JDFTVAL, JDUPSEQ(*DESCEND) and CONCAT keywords aren't supported
      * -For logical files, all fields will be listed
      * -Ref fields are only supported for PFs
      * -Attributes (EDTCDE, etc.) are included for Ref fields even
      *  when inherited from a REF file
      * -SQL data types including UDTs, BLOBs, Datalinks don't have DDS
      *  equivalents
      * -Certain ALIAS names created by an SQL app may be invalid in DDS
      * -Libraries that don't exist on the system will be replaced with
      *  *LIBL in the PFILE and JFILE keywords
      * -Compile time keywords such as RECOVER() and FRCACCPTH() will have
      *  to be gleaned from the existing object.
      * -Select/Omit values are limited to 32 characters
      * -Number of Select/Omit entries is limited to 32
      * -A VIEW's WHERE clause doesn't count as a Select/Omit
      *
      * Compile Options:
      * ===============================================
      * CRTBNDRPG PGM(XXXX/RTVDDSSRCR) SRCFILE(XXXX/QRPGLESRC)
      *
      *================================================================
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      *
      *  File Field Information
      *
     FQADspFFD  IF   E             Disk
      *
      *  Access Path Information
      *
     FQAfdAccP  IF   E             Disk    Usropn
      *
      *  Select/Omit Information
      *
     FQAfdSelO  IF   E             Disk    Usropn
      *
      *  Join Info Information
      *
     FQAfdJoin  IF   E             Disk    Usropn
      *
      *  Record Format Information
      *
     FQAfdRFmt  IF   E             Disk    Usropn
      *
      *  Source Member
      *  Member should be overridden and cleared before calling program
      *
     FQDDSSrc   O  A F   92        Disk
      *=====================================================================
      * Variables
      *=====================================================================
     D aSrcData        S              1    Dim(80)
     D aHeadings       S             80    Dim(13) CTData PerRcd(1)
     D aNumFields      S             10    Dim(500)
      *
     D Len             S              3  0
     D KeyWord         S            512
     D TempKW          S             36
     D FldLength       S              5  0
     D FldLengthA      S              5
     D NoDecPos        S              2
     D Count           S              5  0
     D NumFields       S              5  0
     D MaxKeyN         S                   Like(APKeyN)
     D MaxIBO          S                   Like(WHIBO)
      *
     D PrvHierFile     S             10
     D PrvHierLib      S             10
     D SavRfID         S                   Like(RFID)
     D RfCount         S              3  0
      *
     D Join            S            512
     D PrvJoinFile     S             10
     D PrvJoinLib      S             10
     D PrvJoinNo       S              3  0
     D PrvRcdName      S             10
     D PrvPos          S              3  0
     D I               S              3  0
      *
     D KeyFlag         S              1
     D JoinFlag        S              1
     D HierFlag        S              1
     D SelOmitFlag     S              1
      *
     D SrcSeq          S              6  0
     D SrcDat          S              6  0
     D SrcDta          S             80
      *
      * Entry Parms
      *
     D pFileText       S             50
     D pFileName       S             10
     D pFileAtr        S             10
      *=====================================================================
      * Constants
      *=====================================================================
     D cBlue           C                   Const(x'3A')
     D cNoHeadings     C                   Const(%Elem(aHeadings))
     D cOpt            C                   Const('COMPILE OPTION:')
     D cImmed          C                   Const('MAINT(*IMMED)')
     D cDelay          C                   Const('MAINT(*DLY)')
     D cRebuild        C                   Const('MAINT(*REBLD)')
      *=====================================================================
      * System Data Structure
      *=====================================================================
     D                SDS
     D  sdsPgm                 1     10
     D  sdsUser              254    263
      *=====================================================================
      * Prototype Definitions
      *=====================================================================
     D ReplaceQt       PR           100    Varying
     D   pText                       50    Value
      *
     D ChkLib          PR            10    Varying
     D   pLibrary                    10    Value
      *
     D QCmdExc         PR                  ExtPgm('QCMDEXC')
     D   pCmd                       256    Const
     D   pLen                        15  5 Const
      *
     D Cmd             S            100
      *
      *
      * FILE LEVEL KeyWords
      * -------------------
      *
      * Read Access Path Info and write related information
      *
     C                   Open(E)   QAfdAccP
     C                   If        Not %Error
     C                   Read      QAfdAccP
     C                   If            Not %Eof
     C                             And APNkyF>*Zero
      * Key (Access path) information info exists
     C                   Eval      KeyFlag='Y'
      * Write Comment for Access Path Maintenance
     C                   Eval      aSrcData(7)='*'
     C                   MoveA     cOpt          aSrcData(9)
     C                   Select
     C                   When      apmant='I'
     C                   MoveA     cImmed        aSrcData(25)
     C                   When      apmant='R'
     C                   MoveA     cRebuild      aSrcData(25)
     C                   When      apmant='D'
     C                   MoveA     cDelay        aSrcData(25)
     C                   EndSl
     C                   ExSr      WriteRec
      *
      * APUNIQ          Keys must be unique: N=No, Y=Yes
      *
     C                   If        APUniq='Y'
     C                   Eval      KeyWord='UNIQUE'
     C                   ExSr      WriteKeyWord
     C                   Else
      *
      * APKEYO          L=LIFO, F=FIFO, C=FCFO, N=No specific key order
      *
     C                   Select
     C                   When      APKeyO='L'
     C                   Eval      KeyWord='LIFO'
     C                   ExSr      WriteKeyWord
     C                   When      APKeyO='F'
     C                   Eval      KeyWord='FIFO'
     C                   ExSr      WriteKeyWord
     C                   When      APKeyO='C'
     C                   Eval      KeyWord='FCFO'
     C                   ExSr      WriteKeyWord
     C                   EndSl
     C                   EndIf
      *
     C                   EndIf
      *
      * Test if this is a special "hierarchical" logical file where a
      * single record format shares multiple PFs.
      *
     C                   If        APJoin='N' And APNSCo>1
     C                   Open(E)   QAfdRFmt
     C                   If        Not %Error
     C                   Read      QAfdRFmt
     C                   Dow       Not %EOF
     C                   If        RfID<>SavRFID
     C                   Eval      RfCount=RfCount+1
     C                   Eval      SavRFID=RfID
     C                   EndIf
     C                   Read      QAfdRFmt
     C                   EndDo
     C                   EndIf
      * If the number of distinct formats is < total formats then
      * we have a hierarchical logical file
     C                   If        RfCount<RfTotF
     C                   Eval      HierFlag='Y'
      * Save all PFiles by logical file format name
     C                   EndIf
     C                   EndIf
     C                   EndIf
      *
      * Test if Select/Omit data file exists and has data.
      * Write related file level key words.
      *
     C                   Open(E)   QAfdSelO
     C                   If        Not %Error
     C                   Read      QAfdSelO
     C                   If            Not %Eof
     C                             And SONRul>*Zero
     C                   Eval      SelOmitFlag='Y'
     C                   EndIf
      *
      * Write Dynamic Select Keyword, if present
      *
     C                   If        SODynS='Y'
     C                   Eval      KeyWord='DYNSLT'
     C                   ExSr      WriteKeyWord
     C                   EndIf
     C                   EndIf
      *
      * Read Field Names
      *
     C                   Read      QADspFFD
      *
     C                   Dow       Not %Eof
      *
      * RECORD LEVEL KeyWords
      * ---------------------
      *
      * Check for new record format
      *
     C                   If        WHName<>PrvRcdName
     C                   Eval      MaxIBO=*Zero
     C                   If        PrvRcdName<>*Blanks
     C                   ExSr      KeyFields
     C                   ExSr      SelectOmit
     C                   MoveA     *All' '       aNumFields
     C                   Eval      NumFields=*Zero
     C                   EndIf
      *
     C                   Eval      PrvRcdName=WHName
     C                   Eval      aSrcData(17)='R'
     C                   MoveA     WHName        aSrcData(19)
      *
      * Fill Data Array with Record Format Text Info, if text exists
      * format TEXT key word
      *
     C                   If        WHText<>*Blanks
     C                   Eval      KeyWord='TEXT('''+ReplaceQt(WHText)+''')'
     C                   ExSr      WriteKeyWord
     C                   Else
     C                   ExSr      WriteRec
     C                   EndIf
      *
      * If this file is a logical, write JFILE or PFILE KeyWord
      *
     C                   ExSr      JoinSpec
     C                   ExSr      HierSpec
     C                   If        JoinFlag<>'Y'
     C                             And HierFlag<>'Y'
      *
      * If a physical file is specified, write PFILE KeyWord
      *
     C                   If        APBof<>*Blanks
     C                   ExSr      GetPF
     C                   Eval      KeyWord='PFILE('+ChkLib(APBol)+'/'+
     C                                     %Trim(APBof)+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
     C                   EndIf
      *
     C                   EndIf
      *
      * FIELD LEVEL KeyWords
      * --------------------
      *
      * Fill Data Array with A-Spec Field Info Data
      *
      * Position  6='A'
      * Position 17=Record Indicator
      * Position 19=Field Name
      * Position 29=Ref Indicator
      * Position 30=Field Len
      * Position 35=Data Type
      * Position 36=No Decimal Places
      * Position 45=DDS KeyWords
      *
     C                   If        WHIBO>MaxIBO
     C                   Eval      MaxIBO=WHIBO
     C                   If        WHFldE<>*Blanks
     C                   MoveA     WHFldE        aSrcData(19)
     C                   MoveA     WHFldT        aSrcData(35)
      *
      * If this field is numeric, record it in the array of numeric fields
      * which will be used later when writing out key information.
      *
     C                   If        WHFldD>*Zero
     C                   Eval      NumFields=NumFields+1
     C                   If        NumFields<=%Elem(aNumFields)
     C                   Eval      aNumFields(NumFields)=WHFldE
     C                   EndIf
     C                   EndIf
      *
      * If Not a ref field, output Field Type info
      *
     C                   If            WHRFil<>*Blanks
     C                             And pFileAtr<>'LF'
     C                   Eval      aSrcData(29)='R'
     C                   Eval      aSrcData(35)=' '
     C                   Else
      *
      * Write Data Type and adjust field length, if necessary
      * (For packed, binary & float fields, use actual number digits for
      *  field length rather than number of bytes.)
      *
     C                   Eval      FldLength=*Zero
     C                   Select
      * Packed/Binary/Float
     C                   When         WHFldT='P'
     C                             Or WHFldT='B'
     C                             Or WHFldT='F'
     C                   Eval      FldLength=WHFldd
     C                   When      WHFldT='G'
     C                   Div       2             WHFldB
     C                   Eval      FldLength=WHFldB
      * Date/Time/Timestamp - No Field Length Needed
     C                   When         WHFldT='L'
     C                             Or WHFldT='T'
     C                             Or WHFldT='Z'
     C                   Eval      FldLength=*Zero
      * Varchar
     C                   When          WHFldT='A'
     C                             And WHVarL='Y'
      * For varchar fields, subtract 2 from total # bytes
     C                   Sub       2             WHFldB
     C                   Eval      FldLength=WHFldB
      * Other Data Types, use # bytes
     C                   Other
     C                   Eval      FldLength=WHFldB
     C                   EndSl
      *
     C                   Move      FldLength     FldLengthA
     C                   Eval      FldLengthA=%EditC(FldLength:'Z')
      *
     C                   MoveA     FldLengthA    aSrcData(30)
      *
      * Fill in decimal positions for numeric columns
      *
     C                   If        WHFldD>*Zero
     C                   If        WHFldP=*Zero
     C                   Eval      aSrcData(37)='0'
     C                   Else
     C                   Eval      NoDecPos=%EditC(WHFldP:'Z')
     C                   MoveA     NoDecPos      aSrcData(36)
     C                   EndIf
     C                   EndIf
     C                   EndIf
      *
      * Substring KeyWord
      *
     C                   If        WHMap='Y'
     C                   Eval      KeyWord='SST('+%Trim(WHFldi)+' '+
     C                                     %Trim(%EditC(WHMaps:'Z'))+' '+
     C                                     %Trim(%EditC(WHMapl:'Z'))+')'
     C                   MoveA     KeyWord       aSrcData(45)
     C                   EndIf
      *
      * Write I/O Attribute
      *
     C                   If           (pFileAtr<>'PF' And pFileAtr<>'LF')
     C                             Or WHFiob<>'B'
     C                   Eval      aSrcData(38)=WHFiob
     C                   EndIf
     C                   ExSr      WriteRec
      *
      * Fill Data Array with Field Text Info, if text exists
      * Format TEXT KeyWord
      *
     C                   If        WHFTxt<>*Blanks
     C                   Eval      KeyWord='TEXT('''+ReplaceQt(WHFTxt)+''')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Fill Data Array with VARLEN KeyWord, if Varying length field
      *
     C                   If        WHVarL='Y'
     C                   Eval      KeyWord='VARLEN'
      *
      * Include Allocated Length, if exists
      *
     C                   If        WHAllc<>*Zero
     C                   Eval      KeyWord=%Trim(KeyWord)+
     C                             '('+%Trim(%EditC(whallc:'Z'))+')'
     C                   EndIf
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Fill Data Array with ALIAS KeyWord, if ALIAS name exists
      *
     C                   If        WHAlis<>*Blanks
     C                   Eval      KeyWord='ALIAS('+%TrimR(WHAlis)+')  '
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write Allow Null KeyWord, if field allows nulls
      *
     C                   If        WHNull='Y'
     C                   Eval      KeyWord='ALWNULL'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write CCSID if Alpha and CCSID<>37 (USA)
      *
     C                   If            WHFldT='A'
     C                             And WHCsid<>37
     C                             And WHCsid<>*Zero
     C                             And pFileAtr<>'LF'
     C                   Eval      KeyWord='CCSID('+
     C                                     %Trim(%EditC(WHCsid:'Z'))+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write Date/Time Format & Separator
      *
     C                   If        WHFmt<>*Blanks
     C                   Select
     C                   When      WHFldT='L'
     C                   Eval      KeyWord='DATFMT('+whFmt+')'
     C                   ExSr      WriteKeyWord
     C                   When      WHFldT='T'
     C                   Eval      KeyWord='TIMFMT('+whFmt+')'
     C                   ExSr      WriteKeyWord
     C                   EndSl
     C                   EndIf
      *
      * Add Date/Time Separator KeyWords
      *
     C                   If        WHSep<>*Blanks
     C                   Select
     C                   When      WHFldT='L'
     C                   Eval      KeyWord='DATSEP('''+whSep+''')'
     C                   ExSr      WriteKeyWord
     C                   When      WHFldT='T'
     C                   Eval      KeyWord='TIMSEP('''+WHSep+''')'
     C                   ExSr      WriteKeyWord
     C                   EndSl
     C                   EndIf
      *
      * Write Edit Code or Edit Word
      *
     C                   If            WHECde<>*Blanks
     C                             And WHEWrd=*Blanks
      *
      * Edit code field contains a code and optionally a symbol
      *
     C                   Eval      KeyWord='EDTCDE(' + %Trim(
     C                                      %Subst(WHECde:1:1)+' '+
     C                                      %Subst(WHECde:2:1))+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
     C                   If        WHEWrd<>*Blanks
     C                   Eval      KeyWord='EDTWRD('+%Trim(WHEWrd)+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write Default Value KeyWord if exists
      *
     C                   If            WHDftL>*Zero
     C                             And pFileAtr<>'LF'
     C                   Eval      KeyWord='DFT('+%Subst(WHDft:1:WHDftL)+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write Reference KeyWord
      *
     C                   If            WHRFil<>*Blanks
     C                             And pFileAtr<>'LF'
     C                   Eval      KeyWord='REFFLD(' +
     C                                     %Trim(WHRFmt)+'/'+%Trim(WHRFld)+
     C                                     ' ' +
     C                                     ChkLib(WHRLib)+'/'+%Trim(WHRFil)+
     C                                     ')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write Column Heading KeyWord
      *
     C                   If        WHCHd1+WHCHd2+WHCHd3<>*Blanks
     C                   Eval      KeyWord='COLHDG('''
      * Heading 1
     C                   If        WHCHd1=*Blanks
     C                   Eval      KeyWord=%Trim(KeyWord)+' '''
     C                   Else
     C                   Eval      KeyWord=
     C                             %Trim(KeyWord)+ReplaceQt(WHCHd1)+''''
     C                   EndIf
      * Heading 2
     C                   If        WHCHd2<>*Blanks or WHCHd3<>*Blanks
     C                   If        WHCHd2=*Blanks
     C                   Eval      KeyWord=%Trim(KeyWord)+' '' '''
     C                   Else
     C                   Eval      KeyWord=%Trim(KeyWord)+' '''+
     C                                     ReplaceQt(WHCHd2)+''''
     C                   EndIf
     C                   EndIf
      * Heading 3
     C                   If        WHCHd3<>*Blanks
     C                   Eval      KeyWord=%Trim(KeyWord)+' '''+
     C                                     ReplaceQt(WHCHd3)+''''
     C                   EndIf
     C                   Eval      KeyWord=%Trim(KeyWord)+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write Floating Precision key word if FLOAT type is DOUBLE (8 bytes)
      *
     C                   If        WHFldT='F' And WHFldB=8
     C                   Eval      KeyWord='FLTPCN(*DOUBLE)'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write RENAME KeyWord
      *
     C                   If            WHFldI<>WHFldE
     C                             And WHMap<>'Y'
     C                   Eval      KeyWord='RENAME('+%Trim(WHFldI)+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
      * Write JREF KeyWord
      *
     C                   If        WHJRef<>*Zeros
     C                   Eval      KeyWord='JREF('+%Trim(%EditC(WHJRef:'Z'))+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
     C                   EndIf
      *
     C                   EndIf
      *
      * Read Next Field
      *
     C                   Read      QADspFFD
     C                   EndDo
      *
      * Write Key Field entries
      *
     C                   ExSr      KeyFields
      *
      * Write Select/Omit Field entries
      *
     C                   ExSr      SelectOmit
      *
     C                   Eval      *INLR=*On
     C                   Return
      *================================================================
      * End Mainline
      *================================================================
      *
      * Get PF Name for Select Logical File Record Format
      *
     C     GetPF         BegSr
     C                   If        KeyFlag='Y'
     C     1             Setll     QAfdAccP
     C                   Read      QAfdAccP
     C                   Dow       Not %Eof
     C                   If           APBolF<>PrvRcdName
     C                             Or pFileAtr='PF'
     C                   Eval      APBof=*Blanks
     C                   Else
     C                   Leave
     C                   EndIf
      *
     C                   Read      QAfdAccP
     C                   EndDo
     C                   EndIf
     C                   EndSr
      *================================================================
      * Write Key Field key words for Current LF Record Format or PF
      *================================================================
     C     KeyFields     BegSr
     C                   If        KeyFlag='Y'
      *
     C                   Eval      MaxKeyN=*Zero
     C     1             Setll     QAfdAccP
     C                   Read      QAfdAccP
     C                   Dow       Not %Eof
     C                   If           APBolF=PrvRcdName
     C                             Or pFileAtr='PF'
      *
      * The check to see if the key sequence# repeats is used to
      * weed out duplicate key names in hierarchical logicals
      *
     C                   If        APKeyN>MaxKeyN
     C                   Eval      MaxKeyN=APKeyN
     C                   Eval      aSrcData(17)='K'
     C                   MoveA     APKeyF        aSrcData(19)
      *
      * Descending Key Sequence
      *
     C                   If        APKSeq='D'
     C                   MoveA(P)  'DESCEND'     aSrcData(45)
     C                   EndIf
     C                   ExSr      WriteRec
      *
      * Make sure this field is numeric, before writing numeric keywords
      *
     C     APKeyF        Lookup    aNumFields                             10
     C                   If        %Equal
      *
      * Key Sign
      *
     C                   Select
     C                   When      APKSin='A'
     C                   Eval      KeyWord='ABSVAL'
     C                   When      APKSin='N'
     C                   Eval      KeyWord='UNSIGNED'
     C                   When      APKSin='S'
     C                   Eval      KeyWord='SIGNED'
     C                   EndSl
     C                   ExSr      WriteKeyWord
      *
      * Key Zone/Digit
      *
     C                   Select
     C                   When      APKzd='Z'
     C                   Eval      KeyWord='ZONE'
     C                   ExSr      WriteKeyWord
     C                   When      APKzd='D'
     C                   Eval      KeyWord='DIGIT'
     C                   ExSr      WriteKeyWord
     C                   EndSl
     C                   EndIf
     C                   EndIf
     C                   EndIf
      *
     C                   Read      QAfdAccP
     C                   EndDo
     C                   EndIf
     C                   EndSr
      *================================================================
      * Write Select/Omit key words for current LF record format
      *================================================================
     C     SelectOmit    BegSr
      *
     C                   If        SelOmitFlag='Y'
     C     1             Setll     QAfdSelO
     C                   Read      QAfdSelO
     C                   Dow       Not %Eof And Not %Error
     C                   If        SORFmt=PrvRcdName
      *
     C                   If        SORule='A'
     C                   Eval      SORule=*Blanks
     C                   EndIf
      *
      * Determine Select/Omit comparison type (RANGE KeyWord is
      * replace by a COMP GE and a COMP LE)
      *
     C                   Select
     C                   When      SOComp='AL'
     C                   Eval      KeyWord='ALL'
     C                   When      SOComp='VA'
     C                   Eval      KeyWord='VALUES('
     C                   Other
     C                   Eval      KeyWord='COMP('+%TrimR(SOComp)
     C                   EndSl
      *
     C                   Move      SORule        aSrcData(17)
     C                   MoveA     SOFld         aSrcData(19)
      *
      * Write Comparison Values
      * Select/Omit Comparison Values > 31 characters are truncated
      *
     C                   If        SONVal>*Zero
     C                   Do        SONVal        Count
      *
      * Write ValueS or COMP Values
      *
     C                   Eval      KeyWord=%Trim(KeyWord)+' '+SOValu
      *
      * Loop to read additional Values, if ValueS KeyWord specified
      *
     C                   If        Count<SONVal
     C                   Read      QAfdSelO
     C                   EndIf
     C                   EndDo
     C                   EndIf
      *
      * Close Parenthesis After Value or COMP List
      *
     C                   If           SOComp='VA'
     C                             Or SOComp<>'AL'
     C                   Eval      KeyWord=%TrimR(KeyWord)+')'
     C                   EndIf
     C                   ExSr      WriteKeyWord
      *
      * Bail out of loop early, if this record format has no more
      * rules.  This is to protect duplication of rules for
      * special hierarchical logical files.
      *
     C                   If        SONVal=*Zero
     C                   Leave
     C                   EndIf
     C                   EndIf
      *
     C                   Read(E)   QAfdSelO
     C                   EndDo
     C                   EndIf
     C                   EndSr
      *================================================================
      * Write Special Hierarchical PFile Spec
      *================================================================
     C     HierSpec      BegSr
     C                   If        HierFlag='Y'
     C                   Eval      KeyWord=*Blanks
     C                   Eval      PrvHierFile=*Blanks
     C                   Eval      PrvHierLib=*Blanks
     C     1             Setll     QAfdAccP
     C                   Read      QAfdAccP
     C                   Dow       Not %Eof
      * WHName is current record format
     C                   If        WHName=APBolF
     C                   If           PrvHierFile<>APBOF
     C                             Or PrvHierLib<>APBOL
     C                   Eval      KeyWord=%TrimR(KeyWord)+' '+ChkLib(APBOL)+
     C                                     '/'+%Trim(APBOF)
      *
     C                   Eval      PrvHierFile=APBOF
     C                   Eval      PrvHierLib=APBOL
      *
     C                   EndIf
     C                   EndIf
     C                   Read      QAfdAccP
     C                   EndDo
     C                   Eval      KeyWord='PFILE('+%Trim(KeyWord)+')'
     C                   ExSr      WriteKeyWord
     C                   EndIf
     C                   EndSr
      *================================================================
      * Write Join Spec if this is a Join File
      *================================================================
     C     JoinSpec      BegSr
      *
      * open JOIN info File
      *
     C                   Open(E)   QAfdJoin
     C                   If        Not %Error
     C                   Read      QAfdJoin
      *
      * Test if this is a join file
      *
     C                   If        JNNSpc>*Zero
      *
      * Find Join File Names
      *
      *
     C                   Eval      JoinFlag='Y'
     C                   Eval      KeyWord=*Blanks
      *
      * Loop through all records pulling out each JFILE name
      *
     C                   Dow       Not %Eof
     C                   If           PrvJoinFile<>JndNam
     C                             Or PrvJoinLib<>Jndlnm
     C                             Or PrvJoinNo<>Jndial
     C                   Eval      KeyWord=%TrimR(KeyWord)+' '+ChkLib(Jndlnm)+
     C                                     '/'+%Trim(JndNam)
      *
     C                   Eval      PrvJoinFile=JndNam
     C                   Eval      PrvJoinLib=Jndlnm
     C                   Eval      PrvJoinNo=Jndial
      *
     C                   EndIf
     C                   Read(E)   QAfdJoin
     C                   EndDo
     C                   If           PrvJoinFile<>JndNam
     C                             Or PrvJoinLib<>Jndlnm
     C                             Or PrvJoinNo<>Jndial
     C                   Eval      KeyWord=%TrimR(KeyWord)+' '+ChkLib(Jndlnm)+
     C                                     '/'+%Trim(JndNam)
     C                   EndIf
     C                   Eval      KeyWord='JFILE('+%Trim(KeyWord)+')'
     C                   ExSr      WriteKeyWord
      *
      * Reset File Pointer at 2nd record (1st record isn't useful here)
      *
     C     2             Setll     QAfdJoin
     C                   Read      QAfdJoin
     C                   Eval      aSrcData(17)='J'
      *
      * Write JFLD & JOIN KeyWords
      *
     C                   Eval      PrvPos=Jndial
     C                   Dow       Not %Eof
     C                   If        Jndial<>PrvPos
      * Write JOIN and JDFTSEQ KeyWords for prior join
     C                   If        Join<>*Blanks
     C                   Eval      KeyWord=Join
     C                   ExSr      WriteKeyWord
      * Setup J Reference
     C                   Eval      aSrcData(17)='J'
     C                   EndIf
     C                   Eval      PrvPos=Jndial
     C                   EndIf
      * Join Duplicate Sequence KeyWord
     C                   If        JNJdSq<>*Blanks
     C                   Eval      KeyWord='JDUPSEQ('+%TrimR(JNJdSq)+')'
     C                   ExSr      WriteKeyWord
     C                   Else
      *
      * Write JFLD KeyWord
      *
     C                   Eval      KeyWord='JFLD('+%TrimR(JNJFd1)+' '+
     C                                             %TrimR(JNJFd2)+')'
     C                   ExSr      WriteKeyWord
      * construct JOIN(fromfile:tofile) KeyWord (for use later)
     C                   Eval      Join='JOIN('+%Trim(%EditC(JNJFrm:'Z'))+' '+
     C                                          %Trim(%EditC(JNJTo:'Z'))+')'
     C                   EndIf
      *
     C                   Read(E)   QAfdJoin
     C                   EndDo
      * Write FINAL JOIN and JDFTSEQ KeyWords for prior join
     C                   If        Join<>*Blanks
     C                   Eval      KeyWord=Join
     C                   ExSr      WriteKeyWord
     C                   EndIf
      *
     C                   EndIf
     C                   EndIf
     C                   EndSr
      *=====================================================================
      * Write Record (Data to be written should be in array aSrcData)
      *=====================================================================
     C     WriteRec      BegSr
      *
     C                   Eval      aSrcData(6)='A'
      *
      * For unsupported data types, comment line out
      * 1-BLOB/CLOB, 3-DBCLOB, 4-Datalink
      *
     C                   If            WHFldT>='1'
     C                             And WHFldT<='4'
     C                             And aSrcData(17)<>'R'
     C                   MoveA     '*Unsupp'     aSrcData(7)
     C                   EndIf
      *
      * Move, Write and clear source data line
      *
     C                   MoveA     aSrcData      SrcDta
     C                   Except
     C                   Clear                   SrcDta
     C                   Clear                   aSrcData
      * Increment Seq#
     C                   If        SrcSeq<999999
     C                   Eval      SrcSeq=SrcSeq+1
     C                   EndIf
      *
     C                   EndSr
      *=====================================================================
      * Write KeyWord Record(s)
      *=====================================================================
     C     WriteKeyWord  BegSr
      *
      *
      * Determine Length of KeyWord Field Being Passed
      * The DDS KeyWord space is limited to 36 characters.  Therefore,
      * if the size of a KeyWord exceeds 36 characters then write 35
      * characters at a time along with a line extender (-) until
      * the KeyWord has completed.
      *
     C                   Eval      Len=%Len(%Trim(KeyWord))
     C                   Dow       Len>*Zero
      *
      * Move portion of KeyWord to DDS line
      *
     C                   Eval      TempKW=KeyWord
     C                   If        Len>36
     C                   Move      '-'           TempKW
     C                   Eval      KeyWord=%Subst(Keyword:36)
     C                   Else
     C                   Eval      TempKW=KeyWord
     C                   Eval      KeyWord=*Blanks
     C                   EndIf
      *
     C                   MoveA     TempKW        aSrcData(45)
      *
     C                   ExSr      WriteRec
     C                   Eval      Len=%Len(%Trim(KeyWord))
     C                   EndDo
      *
     C                   EndSr
      *=====================================================================
      * Write Heading Comments / Program ID Section
      *=====================================================================
     C     WriteHdg      BegSr
      *
      * Replace Special Values in Headings
      *
     C                   Eval      %Subst(aHeadings(2):20:50)=pFileText
     C                   Eval      %Subst(aHeadings(4):20:10)=pFileName
     C                   Eval      %Subst(aHeadings(6):20:10)=sdsUser
     C                   Eval      %Subst(aHeadings(8):20:8)=
     C                                                %EditC(udate:'Y')
      *
     C                   Do        cNoHeadings   I
      * Write output to File
     C                   MoveA     aHeadings(I)  aSrcData
     C                   Eval      aSrcData(1)=cBlue
     C                   ExSr      WriteRec
      *
     C                   EndDo
      *
     C                   EndSr
      *=====================================================================
      *
      * Initialization Subroutine
      *
      * Parms:
      * pFileText - Text description from file object
      * pFileName - Name of file object being rebuilt
      * pFileAtr  - Attribute of file being rebuilt
      *
      *=====================================================================
     C     *InzSr        BegSr
     C     *Entry        Plist
     C                   Parm                    pFileText
     C                   Parm                    pFileName
     C                   Parm                    pFileAtr
      *
      * Init Variables
      *
     C                   Eval      SrcSeq=1
     C     10000.01      Mult      UDate         SrcDat
      *
      * Write Heading Text to Member
      *
     C                   ExSr      WriteHdg
      *
     C                   EndSr
      *=====================================================================
      * Source File Record
      *=====================================================================
     OQDDSSrc   EAdd
     O                       SrcSeq               6
     O                       SrcDat              12
     O                       SrcDta              92
      *=====================================================================
      * Replace Quote (')
      *
      * Replace all single quotes with two single quotes so that
      * constants like TEXT & COLHDG which may contain a single
      * quote will compile correctly.
      *
      *=====================================================================
     P ReplaceQt       B
      *
     D ReplaceQt       PI           100    Varying
     D   pText                       50    Value
      *
     D WrkText         S            100    Varying
     D Str             S              3  0
     D pos             S              3  0
      *
     C                   Eval      WrkText=pText
     C                   Eval      Str=1
     C                   Eval      Pos=%Scan('''':WrkText)
     C                   Dow       Pos<>*Zero
     C                   Eval      WrkText=%Subst(WrkText:1:Pos)+''''+
     C                                     %Subst(WrkText:Pos+1)
     C                   Eval      Str=pos+2
     C                   Eval      Pos=%Scan('''':WrkText:Str)
     C                   EndDo
      *
     C                   Eval      WrkText=%TrimR(WrkText)
     C                   Return    WrkText
      *
     P ReplaceQt       E
      *=====================================================================
      * Check Library Name
      *
      * Verify that library name is on the system.  If ref PF library
      * is not on system, then replace library name with *LIBL.
      *
      *=====================================================================
     P ChkLib          B
      *
     D ChkLib          PI            10    Varying
     D   pLibrary                    10    Value
      *
     D RtnLibrary      S             10    Varying
     D SavLibrary      S             10    Static
     D MissingFlg      S              1    Static
      *
      *
     C                   If        savLibrary<>pLibrary
     C                   Eval      savLibrary=pLibrary
     C                   Eval      MissingFlg='N'
      *
     C                   Eval      cmd='CHKOBJ '+%Trim(pLibrary)+' *LIB'
     C                   CallP(E)  QCmdExc(Cmd:64)
     C                   If        %Error
     C                   Eval      MissingFlg='Y'
     C                   EndIf
     C                   EndIf
      *
     C                   If        MissingFlg='Y'
     C                   Eval      pLibrary='*LIBL'
     C                   EndIf
      *
     C                   Eval      RtnLibrary=%Trim(pLibrary)
      *
     C                   Return    RtnLibrary
      *
     P ChkLib          E
**
      *****************************************************************
      *      Desc: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      * File Name: XXXXXXXXXX                                         *
      *                                                               *
      *      User: XXXXXXXXXX                                         *
      *                                                               *
      *      Date: XX/XX/XX                                           *
      *                                                               *
      * This source file was constructed using the RTVDDSSRC          *
      * command.                                                      *
      *                                                               *
      *****************************************************************

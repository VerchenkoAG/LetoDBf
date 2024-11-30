/* 
 * compile to leotoudf.hrb 
 * with: hbmk2 letoudf -gh
 */

#include "rddleto.ch"
#include "dbinfo.ch"
#include "hbsxdef.ch"
#include "set.ch"
/* Default parameters management */
#xcommand DEFAULT <uVar1> := <uVal1> [, <uVarN> := <uValN> ] ;
      => ;
      <uVar1> := iif( <uVar1> == NIL, <uVal1>, <uVar1> ) ;
      [; <uVarN> := iif( <uVarN> == NIL, <uValN>, <uVarN> ) ]

#ifdef __LINUX__
   #define DEF_SEP      '/'
   #define DEF_CH_SEP   '\'
#else
   #define DEF_SEP      '\'
   #define DEF_CH_SEP   '/'
#endif

#define CRLF            chr(13)+chr(10)
#define DEF_TEMP_PATH   "TEMP"


/* function is called immediately after loading letoudf.hrb, if it exists */
FUNCTION UDF_Init()
   RETURN Nil

/* function is needed to use in this HRB a BEGIN SEQUENCE - END SEQUENCE */
STATIC function __BreakBlock()
   RETURN { | oErr | Break( oErr ) }

/* File version of this HRB */
FUNCTION UDF_Version()
   RETURN "3.01"

/* leto_AppGetOptions() of this HRB */
FUNCTION UDF_AppGetOptions
   Local aOptions := {}, cEol := hb_eol()
   Local cHex := iif( cEol == CRLF, "0D0A", "0A" )

   AAdd( aOptions, { "DataPath"       , leto_GetAppOptions(  1 ) } ) // LETOOPT_DATAPATH    
   AAdd( aOptions, { "IndexType"      , leto_GetAppOptions(  2 ) } ) // LETOOPT_INDEXTYPE   
   AAdd( aOptions, { "FileFunc"       , leto_GetAppOptions(  3 ) } ) // LETOOPT_FILEFUNC    
   AAdd( aOptions, { "AnyExt"         , leto_GetAppOptions(  4 ) } ) // LETOOPT_ANYEXT      
   AAdd( aOptions, { "Pass4Login"     , leto_GetAppOptions(  5 ) } ) // LETOOPT_PASS4L      
   AAdd( aOptions, { "Pass4Manage"    , leto_GetAppOptions(  6 ) } ) // LETOOPT_PASS4M      
   AAdd( aOptions, { "Pass4DataModify", leto_GetAppOptions(  7 ) } ) // LETOOPT_PASS4D      
   AAdd( aOptions, { "CryptTraffic"   , leto_GetAppOptions(  9 ) } ) // LETOOPT_CRYPT      
   AAdd( aOptions, { "ShareTables"    , leto_GetAppOptions( 10 ) } ) // LETOOPT_SHARETABLES 
   AAdd( aOptions, { "NoSaveWA"       , leto_GetAppOptions( 11 ) } ) // LETOOPT_NOSAVEWA    
   AAdd( aOptions, { "DebugLevel"     , leto_GetAppOptions( 17 ) } ) // LETOOPT_DEBUGLEVEL  
   AAdd( aOptions, { "Optimize"       , leto_GetAppOptions( 18 ) } ) // LETOOPT_OPTIMIZE    
   AAdd( aOptions, { "AutoOrder"      , leto_GetAppOptions( 19 ) } ) // LETOOPT_AUTOORDER   
   AAdd( aOptions, { "MemoType"       , leto_GetAppOptions( 20 ) } ) // LETOOPT_MEMOTYPE    
   AAdd( aOptions, { "LockCheme"      , leto_GetAppOptions( 22 ) } ) // LETOOPT_LOCKCHEME   
   AAdd( aOptions, { "UdfEnabled"     , leto_GetAppOptions( 23 ) } ) // LETOOPT_UDFENABLED  
   AAdd( aOptions, { "MemoSize"       , leto_GetAppOptions( 24 ) } ) // LETOOPT_MEMOSIZE    
   AAdd( aOptions, { "LowerPath"      , leto_GetAppOptions( 25 ) } ) // LETOOPT_LOWERPATH   
   AAdd( aOptions, { "Trigger"        , leto_GetAppOptions( 26 ) } ) // LETOOPT_TRIGGER     
   AAdd( aOptions, { "HardCommit"     , leto_GetAppOptions( 27 ) } ) // LETOOPT_HARDCOMMIT  
   AAdd( aOptions, { "VersionServer"  , "3.01"                   } )
   AAdd( aOptions, { "VersionUDF"     , UDF_Version()            } )
   AAdd( aOptions, { "VersionOS"      , OS()                     } )
   AAdd( aOptions, { "SepatorPath"    , hb_ps()                  } )
   AAdd( aOptions, { "SepatorLine"    , cHex                     } )
   AAdd( aOptions, { "TempPath"       , DEF_TEMP_PATH            } )

   RETURN aOptions

/* leto_Directory() of this HRB */
FUNCTION UDF_GetFiles( nConn, cPath, cMask, cAttr )
   Local cConn, aFiles, lTempPath, cTempFile, aStru, a, i, k, t
   Local cSec := ltrim(str(Seconds()))
   Local cDataPath := leto_GetAppOptions( 1 )
   Local cTemp := DEF_TEMP_PATH + hb_ps(), cFile
   Local cTempPath := cDataPath + hb_ps() + DEF_TEMP_PATH 
   Local cAls := "_XYZ_" + StrTran(cSec, ".", "")

   IF !hb_IsNumeric( nConn ) ; RETURN "Connection ID not specified"
   ENDIF
   cConn := hb_ntos(nConn)  
   aStru := { {"FILE_NAME", "C", 250, 0}, ;
              {"FILE_SIZE", "N",  18, 0}, ;
              {"DATE_TIME", "T",   8, 0}, ;
              {"FILE_TIME", "C",   8, 0}, ;
              {"FILE_ATTR", "C",   1, 0}  }
   // -------------------------------------------- Temp path
   lTempPath := hb_DirExists( cTempPath )
   IF !lTempPath ; hb_DirBuild( cTempPath )
   ENDIF
   lTempPath := hb_DirExists( cTempPath )
   IF !lTempPath ; cTempPath := cDataPath + hb_ps()
   ENDIF
   // -------------------------------------------- Temp path
   cTempFile := cTempPath + hb_ps() + "_" + cConn + cAls + ".dbf"
   cFile  := cTemp + "_" + cConn + cAls + ".dbf"
   // -------------------------------------------- 
   aFiles := Dir_GetFiles( cPath, cMask, cAttr )
   // -------------------------------------------- 
   IF hb_IsArray(aFiles) .and. Len(aFiles) > 0 .and. hb_IsArray(aFiles[1])
      IF Len(aFiles) > 1
         aFiles := ASort( aFiles, NIL, NIL, {| x, y | x[1] < y[1] } )
      ENDIF
      lTempPath := dbCreate( cTempFile, aStru, , .T., cAls )
      IF lTempPath
         FOR EACH a IN aFiles
             k := Len( a )
             t := iif( Empty(a[5]), "A", a[5] )
             dbAppend()
             FOR i := 1 TO k 
                 IF i < k ; FieldPut( i, a[ i ] )
                 ELSE     ; FieldPut( i, t )
                 ENDIF
             NEXT
         NEXT
         dbGotop()
         k := LastRec()
         dbCloseArea()
         aFiles := {{cFile, k, hb_DateTime(), Time(), "A"}}
      ELSE
         aFiles := "File list not created"
      ENDIF
   ENDIF

RETURN aFiles

STATIC FUNCTION Dir_GetFiles( aPath, aMask, cAttr, aFiles )
   Local aDir, aFil, nLen, cSep := hb_ps(), cPath, cMask, cMsk
   Local cDataPath := leto_GetAppOptions( 1 )

   IF right(cDataPath, 1) != cSep ; cDataPath += cSep
   ENDIF
   IF hb_IsChar( aPath )
      aPath := { aPath }
   ENDIF
   IF hb_IsChar( aMask )
      aMask := iif( "," $ aMask, hb_ATokens(aMask, ","), { aMask } )
   ENDIF
   
   Default aFiles := {}, aPath := {""}, aMask := {"*"}, cAttr := "AD"

   FOR EACH cPath IN aPath
       IF Empty(cPath)
          cPath := ""
       ENDIF
       IF !Empty(cPath) .and. right(cPath, 1) != cSep ; cPath += cSep
       ENDIF
       FOR EACH cMask IN aMask
           cMask := iif( Empty(cMask), "*", alltrim(cMask) )
           cMsk  := "*"
           aDir  := hb_Directory( cDataPath + cPath + cMsk, cAttr )
           IF hb_IsArray(aDir) .and. Len(aDir) > 0
              IF !Empty(cPath) .and. cMsk == cMask
                 nLen := Len(aDir)
                 aFil := { cPath, nLen - 2, hb_CtoT(""), "", "D" }
                 AAdd(aFiles, { aFil[1], aFil[2], aFil[3], aFil[4], aFil[5] })
              ENDIF
              FOR EACH aFil IN aDir
                  IF left(aFil[1], 1) == "."
                     LOOP
                  ENDIF
                  IF aFil[5] == "D"
                     Dir_GetFiles( cPath + aFil[1], cMask, cAttr, aFiles )
                  ELSE
                     IF Empty(aFil[5]) ; aFil[5] := "A"
                     ENDIF
                     IF ( cMsk == cMask .or. hb_FileMatch(aFil[1], cMask) )
                        AAdd(aFiles, { cPath + aFil[1], aFil[2], aFil[3], aFil[4], aFil[5] })
                     ENDIF
                  ENDIF
              NEXT
           ENDIF
       NEXT
   NEXT

   RETURN aFiles

FUNCTION ToLog(...)
   LOCAL s := "", c

   FOR EACH c IN hb_AParams()
       s += hb_valtoexp(c) + chr(9)
   NEXT

   leto_WUsLog( s + CRLF )

   RETURN NIL

FUNCTION A2Log( aDim, nLen )
   LOCAL v
   Default nLen := 5

   FOR EACH v IN aDim
       ToLog( str( hb_enumindex(v), nLen )+". ", hb_valtoexp( v ) )
   NEXT

   RETURN Nil

/* hb_ZipFile() of this HRB */
FUNCTION UDF_ZipFile( nUserID, cFile, acFiles, nLevel, lOverWrite, cPassword, ;
                      lWithPath, lWithDrive, lFullPath, acExclude )
   Local lCompress, cFileZip := "_letotemp.zip", cSep := hb_ps()
   Local cDataPath := leto_GetAppOptions( 1 )
   Local cDataPthU := upper(cDataPath)
   Local cTempPath := DEF_TEMP_PATH + cSep, aVar, cGrp, cFil, a
   Local lBlock := hb_IsNumeric( nUserID ), bUpdate, bProgress

   IF right(cDataPath, 1) != cSep ; cDataPath += cSep
   ENDIF

   IF Empty( cFile ) 
      cFile := iif( lBlock, hb_ntoc( nUserID ), "_" )
      cFile := cTempPath + cFile + cFileZip
   ENDIF

   cFileZip := iif( cDataPthU $ upper(cFile), "", cDataPath ) + cFile 

   IF Empty( nLevel )  ; nLevel := 7
   ENDIF
   IF Empty(lWithPath) ; lWithPath := .T.
   ENDIF

   IF hb_IsChar( acFiles )
      IF !cDataPthU $ upper(acFiles)
         acFiles := cDataPath + acFiles
      ENDIF
   ELSEIF hb_IsArray( acFiles )
      a := {}
      FOR EACH cFil IN acFiles
          IF hb_IsArray(cFil)
             cFil := cFil[1]
          ENDIF
          cFil := StrTran(cFil, DEF_CH_SEP, cSep)
          IF !cDataPthU $ upper(cFil)
             cFil := cDataPath + cFil
          ENDIF
          AAdd(a, cFil )
      NEXT
      acFiles := a
   ENDIF

   IF lBlock
      cGrp := "ZIP_" + hb_ntoc( nUserID )
      aVar := LETO_VARGETLIST( cGrp, .T. )
      IF Len( aVar ) > 0
         Leto_VarSet( cGrp, "cFileName"  , "" )
         Leto_VarSet( cGrp, "nFilePos"   ,  0 )
         Leto_VarSet( cGrp, "nFileReadNr",  0 )
         Leto_VarSet( cGrp, "nFileRead"  ,  0 )
         Leto_VarSet( cGrp, "nFileSize"  ,  0 )
         bUpdate   := {|cfil ,npos | Leto_VarSet(cGrp, "cFileName", cfil ), ;
                                     Leto_VarSet(cGrp, "nFilePos" , npos ), ;
                                     Leto_VarSet(cGrp, "nFileReadNr",  0 ) }
         bProgress := {|nread,nsize, nn| ;
                                     nn := Leto_VarIncr(cGrp, "nFileReadNr"), ;
                                     Leto_VarSet(cGrp, "nFileRead", nread)  , ;
                                     Leto_VarSet(cGrp, "nFileSize", nsize)  }
      ENDIF
   ENDIF

   lCompress := hb_ZipFile( cFileZip , acFiles, nLevel, bUpdate, lOverWrite, ;
                            cPassword, lWithPath, lWithDrive, bProgress,     ;
                            lFullPath, acExclude )
   RETURN lCompress

FUNCTION UDF_HrbRun( cFileHrbName, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 )
   Local uRet := -1, cSep := hb_ps(), cHrbRun
   Local cDataPath := leto_GetAppOptions( 1 ) // in the data directory
   Local cPathHrb  := hb_dirBase()            // next to letodb.exe, letoudf.hrb

   cHrbRun := cPathHrb + cFileHrbName

   IF !hb_FileExists( cHrbRun )
      IF right(cDataPath, 1) != cSep ; cDataPath += cSep
      ENDIF
      cHrbRun := cDataPath + cFileHrbName
      IF !hb_FileExists( cHrbRun ) ; cHrbRun := Nil
      ENDIF
   ENDIF

   IF !Empty( cHrbRun )
      uRet := hb_hrbRun( , cHrbRun, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 )
   ENDIF

   RETURN uRet

/* nFieldPos and xTrigVal are ever empty, except for EVENT_PUT/ EVENT_GET; */
FUNCTION Leto_Trigger( nEvent, nArea, nFieldPos, xTrigVal )

   HB_SYMBOL_UNUSED( nArea )
   HB_SYMBOL_UNUSED( nFieldPos )
   HB_SYMBOL_UNUSED( xTrigVal )

   SWITCH nEvent
   CASE EVENT_PREUSE
      EXIT
   CASE EVENT_POSTUSE
      EXIT
   CASE EVENT_UPDATE
      EXIT
   CASE EVENT_APPEND
      EXIT
   CASE EVENT_DELETE
      EXIT
   CASE EVENT_RECALL
      EXIT
   CASE EVENT_PACK
      EXIT
   CASE EVENT_ZAP
      EXIT
   CASE EVENT_PUT
      EXIT
   CASE EVENT_GET
      EXIT
   CASE EVENT_PRECLOSE
      EXIT
   CASE EVENT_POSTCLOSE
      EXIT
   CASE EVENT_PREMEMOPACK
      EXIT
   CASE EVENT_POSTMEMOPACK
      EXIT
   ENDSWITCH

   RETURN .T.

/*
 *
 * Below you can throw all away, nonsense among, needs a cleanup
 *
 */

/*
 * This sample function demonstrates how to use udf function on Letodb server
 *
 * Function Call from client:
 *
 * cRecno := Leto_Udf("UDF_AppendRec", <cFieldName>, [<cOrder>|<nOrder>], [<xMin>])
 *
 * The function return RECNO() of new record, make a DbGoto( x ) to that at client
 *
 */

FUNCTION UDF_AppendRec( cFieldName, xOrder, xMin )
   LOCAL nPos := FieldPos( cFieldName )
   LOCAL xKey, lApp, lOver := .F.

   IF ! Empty( xOrder )
      OrdSetFocus( xOrder )
   ENDIF
   IF leto_TableLock( 1 )

      dbGoBottom()
      xKey := FieldGet( nPos )
      IF Empty(xKey) .and. ! Empty(xMin)
         xKey := xMin
      ENDIF

      IF ValType( xKey ) == "N"
         xKey ++
         IF hb_FieldType(nPos) $ 'NF'
            lOver := xKey > Val( Replicate( "9", hb_FieldLen( nPos ) ) )
         ELSEIF hb_FieldLen( nPos ) == 2
            lOver := xKey > 0x7FFF
         ELSEIF hb_FieldLen( nPos ) == 4
            lOver := xKey > 0x7FFFFFFF
         ENDIF
      ELSEIF ValType( xKey ) == "C"
         xKey := StrZero( Val(xKey) + 1, Len(xKey) )
         lOver := (xKey = '*')
      ENDIF

      IF lOver
         lApp := .F.
      ELSE
         lApp := ( UDF_Append() != Nil )
      ENDIF

      IF lApp
         FieldPut( nPos, xKey )
         dbCommit()
      ENDIF
      leto_TableUnLock( 1 )
   ELSE
      lApp := .F.
   ENDIF

   RETURN if( lApp, RECNO(), 0 )

FUNCTION UDF_Append()
   LOCAL lApp, lSetDel

   lSetDel := Set( _SET_DELETED, .f. )
   dbGoTop()
   Set( _SET_DELETED, lSetDel )
   IF Deleted() .and. Empty( OrdKeyVal() )
      IF( lApp := leto_RecLock() )
         dbRecall()
      ENDIF
   ELSE
      IF dbAppend()
         IF ( lApp := ! NetErr() )
            leto_RecLock( RecNo(),, .T. )
         ENDIF
      ENDIF
   ENDIF
   RETURN if( lApp, RECNO(), Nil )

/*
 * This sample function delete records on scope xScope, xScopeBottom and filter <cFilter>
 */
FUNCTION UDF_DeleteRecs( xScope, xScopeBottom, xOrder, cFilter, lDeleted )
   LOCAL aRecs := {}, n

   leto_SetEnv( xScope, xScopeBottom, xOrder, cFilter, lDeleted )
   dbEval({|| AADD(aRecs, RecNo())})
   leto_ClearEnv( xScope, xScopeBottom, xOrder, cFilter )

   FOR EACH n in aRecs
      dbGoto(n)
      IF leto_RecLock( n )
         ClearRec()
         leto_RecUnlock( n )
      ENDIF
   NEXT
   dbCommit()

   RETURN RECNO()

/*
 * This sample function delete records and clears table fields
 */
FUNCTION UDF_ClearRec( lDelete, nRec )
   Local lClearRec := .F.

   IF Empty(nRec) ; nRec := RecNo()
   ENDIF
   IF nRec != RecNo() ; dbGoto( nRec )
   ENDIF

   IF leto_RecLock( nRec )
      ClearRec( lDelete )
      leto_RecUnlock( nRec )
      lClearRec := .T.
   ENDIF

   RETURN lClearRec

STATIC FUNCTION ClearRec( lDelete )
   LOCAL nCount := FCount(), nLoop, xValue

   lDelete := ( lDelete == NIL .or. !Empty( lDelete ) )

   IF lDelete 
      dbDelete()
   ENDIF

   FOR nLoop := 1 to nCount
      xValue := Nil
      SWITCH HB_FIELDTYPE( nLoop )
         CASE "C"
         CASE "M"
            xValue := ""
            EXIT
         CASE "N"
         CASE "F"
         CASE "I"
         CASE "Y"
         CASE "Z"
         CASE "2"
         CASE "4"
         CASE "8"
         CASE "B"
            xValue := 0
            EXIT
         CASE "L"
            xValue := .F.
            EXIT
         CASE "D"
         CASE "T"
            xValue := CTOD( "" )
            EXIT
      ENDSWITCH
      IF xValue != Nil
         FieldPut( nLoop, xValue )
      ENDIF
   NEXT
   RETURN Nil

/*
 * UDF_FilesExist - check files existence at the specified path
 * Parameters:
 * cPaths - list of directories, delimited with comma
 * aFiles - array of filenames without path to check
 *
 * This function return array of path for each file or "-" symbol,
 * if file doesn't exist
 */
FUNCTION UDF_FilesExist( cPaths, aFiles)
   LOCAL aRet := {}, cFile, lFound, cPath
   LOCAL cDataPath := leto_GetAppOptions( 1 )
   LOCAL aPath := leto_getPath( cPaths )

   for each cFile in aFiles
      lFound := .f.
      for each cPath in aPath
         if File( StrTran( cDataPath + cPath + cFile, DEF_CH_SEP, DEF_SEP ) )
            AADD( aRet, cPath )
            lFound := .t.
            exit
         endif
      next
      if ! lFound
         AADD( aRet, "-" )
      endif
   next

   RETURN aRet

STATIC FUNCTION leto_getPath( cPaths )
   LOCAL aPath := hb_ATokens( cPaths, "," ), nI
   for nI := 1 to len( aPath )
      if ! ( Right( aPath[ nI ], 1) $ DEF_CH_SEP + DEF_SEP )
         aPath[ nI ] += DEF_CH_SEP
      endif
   next
   RETURN aPath

/*
 * UDF_Locate function locate record on scope xScope, xScopeBottom and filter <cFilter>
   If lLast parameter specified, function locate the last occurence of record.
   If record isn't found, UDF_Locate returns eof() value.
 */
FUNCTION UDF_Locate( xScope, xScopeBottom, xOrder, cFilter, lDeleted, lLast )

   leto_SetEnv( xScope, xScopeBottom, xOrder, cFilter, lDeleted )

   IF lLast == Nil
      GO TOP
   else
      GO BOTTOM
   endif

   leto_ClearEnv( xScope, xScopeBottom, xOrder, cFilter )

   RETURN RECNO()

FUNCTION UDF_dbEval( xScope, xScopeBottom, xOrder, cFilter, lDeleted )
   LOCAL cRecs

   leto_SetEnv( xScope, xScopeBottom, xOrder, cFilter, lDeleted )
   GO TOP
   cRecs := leto_dbEval()
   leto_ClearEnv( xScope, xScopeBottom, xOrder, cFilter )

   RETURN cRecs

FUNCTION UDF_Trans( cTo )
/*
 * UDF_Trans copy all records from current area to area with <cTo> client alias
   with conversion between numeric and character fields.
*/
   LOCAL cArea := Alias()
   LOCAL cAliasTo := leto_Alias( cTo )
   LOCAL lSetDel
   LOCAL lRes := .T., oError

   dbSelectArea( cArea )
   IF ! Empty( cAliasTo )
      lSetDel := Set( _SET_DELETED, .f. )
      BEGIN SEQUENCE WITH { | e | break( e ) }
         OrdSetFocus( 0 )
         GO TOP
         WHILE ! eof()
            UDF_TransRec( cAliasTo )
            SKIP
         ENDDO
         (cAliasTo)->(dbCommit())
      RECOVER USING oError
         WUsLog( "UDF_Trans error: " + cArea + "-->" + cTo + " " +;
                oError:description + if( ! Empty( oError:operation ), ":" + oError:operation, "" ) +;
                " recno " + LTrim(Str(RecNo())))
         lRes := .F.
      END SEQUENCE
      Set( _SET_DELETED, lSetDel )
   ENDIF
   RETURN lRes

STATIC FUNCTION UDF_TransRec( cAliasTo )
   LOCAL nPos1, nPos2, xVal, cFName, ct1, ct2

   (cAliasTo)->( dbAppend() )
   IF ! NetErr()
      FOR nPos1 := 1 to FCount()
         cFName := FieldName( nPos1 )
         IF (nPos2 := (cAliasTo)->(FieldPos(cFName))) # 0 .and. ! (cAliasTo)->(hb_FieldType(nPos2)) $ '+^'
            xVal := (cAliasTo)->(FieldGet(nPos2))
            ct2 := ValType( xVal )
            xVal := FieldGet( nPos1 )
            IF ! Empty( xVal )
               ct1 := ValType( xVal )
               IF ct2 = ct1
                  IF ct1 == "C"
                     xVal := RTrim( xVal )
                  ENDIF
                  (cAliasTo)->(FieldPut( nPos2, xVal ))
               ELSEIF ct1 = 'C' .and. ct2 = 'N'
                  (cAliasTo)->(FieldPut( nPos2, Val(xVal)))
               ELSEIF ct1 = 'N' .and. ct2 = 'C'
                  (cAliasTo)->(FieldPut( nPos2, Str(xVal, hb_FieldLen(nPos2), hb_FieldDec(nPos2))))
               ENDIF
            ENDIF
         ENDIF
      NEXT
      IF Deleted()
         (cAliasTo)->( dbDelete() )
      ENDIF
   ENDIF
   RETURN Nil

FUNCTION UDF_OpenTables( aTables, cPaths )
/*
 * UDF_OpenTables open a tables, described in aTables array,
   and return to client an array with structure of opened tables
   Each table described with array of (at least) 1 to 5 elements:
   {<cFileName>, [<cAlias>], [<lShared>], [<lReadOnly>], [<cdp>]}
   Tables are opened on the server by one request from the client,
   the server returns information on open tables in the array,
   and then elements of array is transferred to the "use" command.
   The "use" command opens tables without the request to the server.
   The <alias> in the "use" command is mandatory parameter.

   Example of usage UDF_OpenTables from client:

   if leto_UDFExist( "UDF_OpenTables" )

      aAreas := leto_UDF( ""UDF_OpenTables"", {{"table1",, .t.}, {"table2",, .t.}, {"table3",, .t.}} )
      use (aAreas[1]) alias table1 shared new
      use (aAreas[2]) alias table2 shared new
      use (aAreas[3]) alias table3 shared new

   else

      use table1 alias table1 shared new
      use table2 alias table2 shared new
      use table3 alias table3 shared new

   endif

 */
   LOCAL aOpen := {}, aItem, nLen, cTable
   LOCAL cDataPath, aPath, cPath, cOpen

   IF cPaths != nil
      cDataPath := leto_GetAppOptions( 1 )
      aPath := leto_getPath( cPaths )
   ENDIF

   FOR EACH aItem IN aTables
      cTable := aItem[1]
      IF ! Empty( aPath )
         FOR EACH cPath IN aPath
            IF File( StrTran( cDataPath + cPath + cTable + ".dbf", DEF_CH_SEP, DEF_SEP ) )
               cTable := cPath + cTable
               exit
            ENDIF
         NEXT
      ENDIF
      nLen := len( aItem )
      /* leto_USE() deprecated ==> Leto_DbUseArea() */
      cOpen := leto_DbUseArea( cTable,;
                               IIF( nLen >=2, aItem[2], ),;
                               IIF( nLen >=3, aItem[3], ),;
                               IIF( nLen >=4, aItem[4], ),;
                               IIF( nLen >=5, aItem[5], ) )
      AADD( aOpen, "+" + cTable + ";" + Substr( cOpen, Asc( Left( cOpen, 1 ) ) + 2 ) )
   NEXT

   RETURN aOpen

FUNC RPC_THREAD( nSeconds )
   IF VALTYPE( nSeconds ) != "N" .OR. nSeconds <= 0
      nSeconds := 42
   ENDIF
   WHILE nSeconds > 0 .AND. ! leto_UDFMustQuit()
      hb_idleSleep( 1 )
      nSeconds--
   ENDDO

   RETURN nSeconds
   

/* leto_dbcreate() needs server option No_Save_WA = 1 */
FUNC elktest
  LOCAL cReturn := leto_Alias()
  LOCAL cAlias := leto_Alias( "ELK" )

  cReturn += "==" + cAlias  + ";"

  IF EMPTY( cAlias )
     IF leto_dbcreate( "mem:elk",{{"name","C",32,0},{"age","N",3,0}},"DBFCDX",.T.,"ELK")
        leto_ordcreate( "ELK", "mem:elk", "name", "NAME" )
        cReturn += leto_alias("ELK") + "#"
        leto_ordcreate( "ELK", "mem:elk", "age", "AGE" )
        leto_dbclosearea()
     ENDIF
  ENDIF
RETURN cReturn


FUNC srvsel
   LOCAL cRet := ""
   LOCAL i := 1

   DO WHILE i < 256
      DBSELECTAREA( i )
      IF ! EMPTY( ALIAS() )
         cRet += STR( i,3,0 ) + "=" + ALIAS() + ";"
      ENDIF
      i++
   ENDDO
RETU cRet

/* leto_dbcreate() needs server option No_Save_WA = 1 */
FUNC rpcelk
  IF leto_dbcreate( "mem:elk",{ { "name", "C", 32, 0 }, { "age", "N", 3, 0 } }, "DBFCDX", .T., "ELK" )
    WUsLog( "rpcelk create ")
    leto_OrdCreate( "mem:elk", "name", "NAME" )
    leto_OrdCreate( "mem:elk", "age", "AGE" )
    WUsLog( "rpcelk create ")
    leto_DbCloseArea()
    RETURN .T.
  ENDIF
RETURN .F.


FUNC leto_ResultHead( cFields )
   LOCAL bAll
   LOCAL nFieldsLen
   LOCAL nFields, aFields
   LOCAL aHeads
   LOCAL nFound := 0
   LOCAL aStruct := DBSTRUCT()
   LOCAL i, nPos

   hb_default( @cFields, "*" )

   bAll := cFields == "*"
   IF ! bAll
      nFieldsLen := LEN( cFields )
      IF RIGHT( cFields, 1 ) == ","
         nFieldsLen--
      ENDIF
      cFields := UPPER( LEFT( cFields, nFieldsLen ) )
      aFields := hb_ATokens( cFields, "," )
      nFields := LEN( aFields )
   ELSE
      nFields := FCOUNT()
   ENDIF
   aHeads := ARRAY( nFields )

   FOR i := 1 TO nFields
      IF bAll
         nPos := i
      ELSE
         nPos := FIELDPOS( aFields[ i ] )
      ENDIF
      IF nPos > 0
         nFound++
         aHeads[ nFound ] := ACLONE( aStruct[ nPos ] )
      ENDIF
   NEXT i
   IF nFound != LEN( aHeads )
      ASIZE( aHeads, nFound )
   ENDIF
RETURN aHeads

FUNC leto_ResultRow( cFields )
   LOCAL bAll
   LOCAL nFieldsLen
   LOCAL nFields, aFields
   LOCAL aRow
   LOCAL nFound := 0
   LOCAL i, nPos

   hb_default( @cFields, "*" )
   bAll := cFields == "*"
   IF ! bAll
      nFieldsLen := LEN( cFields )
      IF RIGHT( cFields, 1 ) == ","
         nFieldsLen--
      ENDIF
      cFields := UPPER( LEFT( cFields, nFieldsLen ) )
      aFields := hb_ATokens( cFields, "," )
      nFields := LEN( aFields )
   ELSE
      nFields := FCOUNT()
   ENDIF
   aRow := ARRAY( nFields )

   FOR i := 1 TO nFields
      IF bAll
         nPos = i
      ELSE
         nPos := FIELDPOS( aFields[ i ] )
      ENDIF
      IF nPos > 0
         nFound++
         aRow[ nFound ] := FIELDGET( nPos )
      ENDIF i
   NEXT i
   IF nFound != LEN( aRow )
      ASIZE( aRow, nFound )
   ENDIF
RETURN aRow


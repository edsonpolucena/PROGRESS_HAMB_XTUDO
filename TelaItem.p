
DEFINE INPUT        PARAMETER ipAcao     AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioCodProd  AS INTEGER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioQtd      AS INTEGER   NO-UNDO.
DEFINE OUTPUT       PARAMETER opValUnit  AS DECIMAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opValTotal AS DECIMAL   NO-UNDO.

DEFINE BUFFER bProd FOR Produtos.

DEFINE VARIABLE vCodProd   AS INTEGER                       NO-UNDO.
DEFINE VARIABLE vQtd       AS INTEGER                       NO-UNDO.
DEFINE VARIABLE vcProduto  AS CHARACTER FORMAT "x(60)"      NO-UNDO.
DEFINE VARIABLE dValorUnit AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE dTotal     AS DECIMAL   FORMAT ">>>>>>9.99" NO-UNDO.

DEFINE BUTTON bt-saveItem LABEL "Salvar".
DEFINE BUTTON bt-cancItem LABEL "Cancelar" AUTO-ENDKEY.

ASSIGN
  vCodProd = ioCodProd
  vQtd     = ioQtd.

IF vCodProd > 0 THEN DO:
  FIND bProd WHERE bProd.CodProduto = vCodProd NO-LOCK NO-ERROR.
  IF AVAILABLE bProd THEN DO:
    ASSIGN
      vcProduto  = bProd.NomProduto
      dValorUnit = bProd.ValProduto
      dTotal     = (IF vQtd > 0 THEN dValorUnit * vQtd ELSE 0).
  END.
END.

DEFINE FRAME fItem
  vCodProd   LABEL "Produto"     COLON 20 vcProduto NO-LABEL
  vQtd       LABEL "Quantidade"  COLON 20
  dTotal     LABEL "Valor Total" COLON 20 SKIP(1)
  bt-saveItem bt-cancItem
  WITH SIDE-LABELS THREE-D
       VIEW-AS DIALOG-BOX TITLE "Item"
       SIZE 80 BY 10.


ON VALUE-CHANGED OF vCodProd IN FRAME fItem DO:
  DEFINE VARIABLE cCod AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCod AS INTEGER   NO-UNDO.

  ASSIGN cCod = vCodProd:SCREEN-VALUE IN FRAME fItem.

  DO ON ERROR UNDO, LEAVE:
    ASSIGN iCod = INTEGER(cCod).
  END.
  IF ERROR-STATUS:ERROR THEN RETURN.

  ASSIGN vCodProd = iCod.

  FIND bProd WHERE bProd.CodProduto = vCodProd NO-LOCK NO-ERROR.
  IF AVAILABLE bProd THEN DO:
    ASSIGN
      vcProduto  = bProd.NomProduto
      dValorUnit = bProd.ValProduto
      dTotal     = (IF vQtd > 0 THEN dValorUnit * vQtd ELSE 0).
  END.
  ELSE DO:
    ASSIGN vcProduto = "" dValorUnit = 0 dTotal = 0.
  END.

  DISPLAY vcProduto dTotal WITH FRAME fItem.
END.

ON VALUE-CHANGED OF vQtd IN FRAME fItem DO:
  DEFINE VARIABLE cQtd AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iQtd AS INTEGER   NO-UNDO.

  ASSIGN cQtd = vQtd:SCREEN-VALUE IN FRAME fItem.

  DO ON ERROR UNDO, LEAVE:
    ASSIGN iQtd = INTEGER(cQtd).
  END.
  IF ERROR-STATUS:ERROR THEN RETURN.

  ASSIGN vQtd   = iQtd
         dTotal = (IF vQtd > 0 THEN dValorUnit * vQtd ELSE 0).

  DISPLAY dTotal WITH FRAME fItem.
END.

ON CHOOSE OF bt-saveItem IN FRAME fItem DO:
  DEFINE VARIABLE cCod AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cQtd AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCod AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iQtd AS INTEGER   NO-UNDO.

  ASSIGN
    cCod = vCodProd:SCREEN-VALUE IN FRAME fItem
    cQtd = vQtd:SCREEN-VALUE     IN FRAME fItem.

  DO ON ERROR UNDO, LEAVE:
    ASSIGN
      iCod = INTEGER(cCod)
      iQtd = INTEGER(cQtd).
  END.
  IF ERROR-STATUS:ERROR OR iCod <= 0 OR iQtd <= 0 THEN DO:
    MESSAGE "Preencha produto e quantidade corretamente."
      VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  FIND bProd WHERE bProd.CodProduto = iCod NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bProd THEN DO:
    MESSAGE "Produto inexistente." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  ASSIGN
    vCodProd   = iCod
    vQtd       = iQtd
    dValorUnit = bProd.ValProduto
    dTotal     = dValorUnit * vQtd.

  ASSIGN
    ioCodProd   = vCodProd
    ioQtd       = vQtd
    opValUnit   = dValorUnit
    opValTotal  = dTotal.

  APPLY "END-ERROR" TO FRAME fItem.  
  RETURN "OK".
END.

ON CHOOSE OF bt-cancItem IN FRAME fItem DO:
  APPLY "END-ERROR" TO FRAME fItem.
  RETURN "CANCEL".
END.

DISPLAY vCodProd vcProduto vQtd dTotal WITH FRAME fItem.
DO WITH FRAME fItem:
  ENABLE vCodProd vQtd bt-saveItem bt-cancItem.
END.

WAIT-FOR END-ERROR OF FRAME fItem.
RETURN.

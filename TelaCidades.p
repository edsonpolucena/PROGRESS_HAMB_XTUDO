USING Progress.JSON.ObjectModel.*.

DEFINE QUERY qCid FOR cidades SCROLLING.

DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.

DEFINE BUTTON bt-pri  LABEL "<<".       
DEFINE BUTTON bt-ant  LABEL "<".        
DEFINE BUTTON bt-prox LABEL ">".        
DEFINE BUTTON bt-ult  LABEL ">>".       
DEFINE BUTTON bt-add  LABEL "Novo".     
DEFINE BUTTON bt-mod  LABEL "Modificar".
DEFINE BUTTON bt-del  LABEL "Remover".  
DEFINE BUTTON bt-save LABEL "Salvar".   
DEFINE BUTTON bt-canc LABEL "Cancelar".
DEFINE BUTTON btExport LABEL "Exportar".
DEFINE BUTTON bt-sair LABEL "Sair" AUTO-ENDKEY.

DEFINE FRAME fCid
    bt-pri  AT 10
    bt-ant  
    bt-prox 
    bt-ult  
    bt-add  
    bt-mod 
    bt-del  
    bt-save 
    bt-canc
    btExport 
    bt-sair SKIP(1)

    cidades.codcidade COLON 20
    cidades.nomcidade COLON 20
    cidades.codUF     COLON 20

    WITH SIDE-LABELS THREE-D SIZE 140 BY 15
         VIEW-AS DIALOG-BOX TITLE "Cidades".

ENABLE ALL WITH FRAME fCid.

ON CHOOSE OF bt-pri  DO: GET FIRST qCid. RUN piMostraCid. END.
ON CHOOSE OF bt-ant  DO: GET PREV  qCid. RUN piMostraCid. END.
ON CHOOSE OF bt-prox DO: GET NEXT  qCid. RUN piMostraCid. END.
ON CHOOSE OF bt-ult  DO: GET LAST  qCid. RUN piMostraCid. END.

ON CHOOSE OF bt-add DO:
  ASSIGN cAction = "add".
  RUN piHabilitaBotoesCid(FALSE).
  RUN piHabilitaCamposCid(TRUE).

  CLEAR FRAME fCid.
  DISPLAY NEXT-VALUE(seqCidade) @ cidades.codcidade WITH FRAME fCid.
END.

ON CHOOSE OF bt-mod DO:
  ASSIGN cAction = "mod".
  RUN piHabilitaBotoesCid(FALSE).
  RUN piHabilitaCamposCid(TRUE).
  DISPLAY cidades.codcidade WITH FRAME fCid.
  RUN piMostraCid.
END.

ON CHOOSE OF bt-del DO:
  DEFINE VARIABLE lConf AS LOGICAL NO-UNDO.
  DEFINE BUFFER bCidade FOR cidades.
  

  MESSAGE "Confirma a exclusao da cidade " STRING(cidades.codcidade) "?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      TITLE "Excluir" UPDATE lConf.
  IF NOT lConf THEN 
    RETURN NO-APPLY.

  IF CAN-FIND(FIRST clientes
              WHERE clientes.codcidade = cidades.codcidade NO-LOCK) THEN DO:
    MESSAGE "Não e possivel excluir: existem clientes nesta cidade."
        VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  FIND bCidade
       WHERE bCidade.codcidade = cidades.codcidade
       EXCLUSIVE-LOCK NO-ERROR.

  IF AVAILABLE bCidade THEN DO:
    DELETE bCidade.

    RUN piOpenQueryCid.

    DO:
      GET FIRST qCid.
      IF AVAILABLE cidades THEN 
        RUN piMostraCid.
      ELSE DO:
        CLEAR FRAME fCid.
      END.
    END.
  END.
END.




ON CHOOSE OF bt-save DO:
    DEFINE BUFFER bCidade FOR cidades.
    
  IF cAction = "add" THEN DO:
    CREATE bCidade.
    ASSIGN bCidade.codcidade = INPUT cidades.codcidade.
  END.
  ELSE FIND bCidade
         WHERE bCidade.codcidade = cidades.codcidade
         EXCLUSIVE-LOCK NO-ERROR.

  ASSIGN
    bCidade.nomcidade = INPUT cidades.nomcidade
    bCidade.codUF     = INPUT cidades.codUF.

  RUN piHabilitaBotoesCid(TRUE).
  RUN piHabilitaCamposCid(FALSE).
  RUN piOpenQueryCid.
END.

ON CHOOSE OF bt-canc DO:
  RUN piHabilitaBotoesCid(TRUE).
  RUN piHabilitaCamposCid(FALSE).
  RUN piMostraCid.
END.

ON CHOOSE OF btExport DO:
  DEFINE VARIABLE cCsv AS CHARACTER NO-UNDO.
  ASSIGN cCsv = "cidades.csv".
  OUTPUT TO VALUE(cCsv).
  PUT UNFORMATTED "CodCidade;NomCidade" SKIP.
  FOR EACH cidades NO-LOCK BY cidades.CodCidade:
    PUT UNFORMATTED
      STRING(cidades.CodCidade) ";"
      QUOTER(cidades.NomCidade) SKIP.
  END.
  OUTPUT CLOSE.

  DEFINE VARIABLE cJson AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ja    AS JsonArray  NO-UNDO.
  DEFINE VARIABLE jo    AS JsonObject NO-UNDO.

  ASSIGN cJson = "cidades.json" ja = NEW JsonArray().
  FOR EACH cidades NO-LOCK BY cidades.CodCidade:
    jo = NEW JsonObject().
    jo:Add("CodCidade", cidades.CodCidade).
    jo:Add("NomCidade", cidades.NomCidade).
    ja:Add(jo).
  END.
  ja:WriteFile(cJson).
  DELETE OBJECT ja.

  OS-COMMAND SILENT VALUE("notepad " + cCsv).
  OS-COMMAND SILENT VALUE("notepad " + cJson).
END.


RUN piOpenQueryCid.
RUN piHabilitaCamposCid(FALSE).
RUN piMostraCid.

WAIT-FOR WINDOW-CLOSE OF FRAME fCid.

RETURN.


PROCEDURE piMostraCid:
  IF AVAILABLE cidades THEN
    DISPLAY cidades.codcidade cidades.nomcidade cidades.codUF WITH FRAME fCid.
  ELSE
    CLEAR FRAME fCid.
END PROCEDURE.

PROCEDURE piOpenQueryCid:
    DEFINE VARIABLE rRecord AS ROWID       NO-UNDO.
    
    IF  AVAILABLE cidades THEN DO:
        ASSIGN rRecord = ROWID(cidades).
    END.
    
    OPEN QUERY qCid 
        FOR EACH cidades BY Cidades.CodCidade. 
    IF rRecord <> ? THEN
    REPOSITION qCid TO ROWID rRecord NO-ERROR.
  
END PROCEDURE.


PROCEDURE piHabilitaBotoesCid:
  DEFINE INPUT PARAMETER pEnable AS LOGICAL NO-UNDO.
  DO WITH FRAME fCid:
    ASSIGN
      bt-pri:SENSITIVE  = pEnable
      bt-ant:SENSITIVE  = pEnable
      bt-prox:SENSITIVE = pEnable
      bt-ult:SENSITIVE  = pEnable
      bt-add:SENSITIVE  = pEnable
      bt-mod:SENSITIVE  = pEnable
      bt-del:SENSITIVE  = pEnable
      bt-save:SENSITIVE = NOT pEnable
      bt-canc:SENSITIVE = NOT pEnable
      bt-sair:SENSITIVE = pEnable.
  END.
END PROCEDURE.

PROCEDURE piHabilitaCamposCid:
  DEFINE INPUT PARAMETER pEnable AS LOGICAL NO-UNDO.
  DO WITH FRAME fCid:
    ASSIGN
      cidades.codcidade:SENSITIVE = FALSE
      cidades.nomcidade:SENSITIVE = pEnable
      cidades.codUF:SENSITIVE     = pEnable.
  END.
END PROCEDURE.

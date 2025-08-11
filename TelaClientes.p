USING Progress.JSON.ObjectModel.*.

DEFINE QUERY qCli FOR clientes SCROLLING.

DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcCidade AS CHARACTER FORMAT "x(30)" NO-UNDO.

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

DEFINE FRAME fCli
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

    Clientes.CodCliente     COLON 20
    Clientes.NomCliente     COLON 20
    Clientes.CodEndereco    COLON 20
    Clientes.CodCidade      COLON 20 vcCidade                NO-LABEL
    Clientes.Observacao     COLON 20
    WITH SIDE-LABELS THREE-D SIZE 140 BY 15
         VIEW-AS DIALOG-BOX TITLE "Clientes".

ENABLE ALL WITH FRAME fCli.

DEFINE BUFFER bCidade FOR cidades.

/* Valida cidade ao sair do campo */
ON LEAVE OF Clientes.CodCidade IN FRAME fCli DO:
  DEFINE BUFFER   bCidade FOR cidades.
  DEFINE VARIABLE iCodCid AS INTEGER NO-UNDO.

  /* lê o que está digitado no widget */
  ASSIGN iCodCid = INTEGER(SELF:SCREEN-VALUE) NO-ERROR.

  IF ERROR-STATUS:ERROR OR iCodCid <= 0 THEN DO:
    MESSAGE "Informe um codigo de cidade válido." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY. /* mantém o foco no campo */
  END.

  FIND bCidade WHERE bCidade.CodCidade = iCodCid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bCidade THEN DO:
    MESSAGE "Codigo de cidade " iCodCid " nao encontrado." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  ASSIGN vcCidade = bCidade.NomCidade.
  DISPLAY vcCidade WITH FRAME fCli.
END.


ON CHOOSE OF bt-pri  DO: GET FIRST qCli. RUN piMostraCli. END.
ON CHOOSE OF bt-ant  DO: GET PREV  qCli. RUN piMostraCli. END.
ON CHOOSE OF bt-prox DO: GET NEXT  qCli. RUN piMostraCli. END.
ON CHOOSE OF bt-ult  DO: GET LAST  qCli. RUN piMostraCli. END.

ON CHOOSE OF bt-add DO:
  ASSIGN cAction = "add".
  RUN piHabilitaBotoesCli(FALSE).
  RUN piHabilitaCamposCli(TRUE).

  CLEAR FRAME fCli.
  DISPLAY NEXT-VALUE(seqCliente) @ Clientes.CodCliente WITH FRAME fCli.
END.

ON CHOOSE OF bt-mod DO:
  ASSIGN cAction = "mod".
  RUN piHabilitaBotoesCli(FALSE).
  RUN piHabilitaCamposCli(TRUE).
  DISPLAY Clientes.CodCliente WITH FRAME fCli.
  RUN piMostraCli.
END.

ON CHOOSE OF bt-del DO:
  DEFINE VARIABLE lConf AS LOGICAL NO-UNDO.
  DEFINE BUFFER bCliente FOR clientes.

  MESSAGE "Confirma a exclusao da cliente " STRING(Clientes.CodCliente) "?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      TITLE "Excluir" UPDATE lConf.
  IF NOT lConf THEN RETURN NO-APPLY.

  FIND bCliente WHERE bCliente.CodCliente = Clientes.CodCliente EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE bCliente THEN DO:
    DELETE bCliente.
    RUN piOpenQueryCli.
    DO:
      GET FIRST qCli.
      IF AVAILABLE clientes THEN 
        RUN piMostraCli.
      ELSE DO:
        CLEAR FRAME fCli.
      END.
    END.
  END.
END.

ON CHOOSE OF bt-save DO:
  DEFINE BUFFER bCliente FOR clientes.
  DEFINE BUFFER bCidade  FOR cidades.
  DEFINE VARIABLE rSaved  AS ROWID   NO-UNDO.
  DEFINE VARIABLE iCodCid AS INTEGER NO-UNDO.

  /* ===== valida a cidade ===== */
  ASSIGN iCodCid = INPUT Clientes.CodCidade NO-ERROR.
  IF ERROR-STATUS:ERROR OR iCodCid <= 0 THEN DO:
    MESSAGE "Informe um codigo de cidade valido." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  FIND bCidade WHERE bCidade.CodCidade = iCodCid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bCidade THEN DO:
    MESSAGE "Cidade " iCodCid " nao encontrada. Não e possivel salvar."
            VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  /* ===== gravação ===== */
  IF cAction = "add" THEN DO:
    CREATE bCliente.
    ASSIGN bCliente.CodCliente = INPUT Clientes.CodCliente.
  END.
  ELSE DO:
    FIND bCliente WHERE bCliente.CodCliente = Clientes.CodCliente
         EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bCliente THEN DO:
      MESSAGE "Registro nao encontrado para salvar." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
  END.

  ASSIGN
    bCliente.NomCliente  = INPUT Clientes.NomCliente
    bCliente.CodEndereco = INPUT Clientes.CodEndereco
    bCliente.CodCidade   = iCodCid        /* já validado */
    bCliente.Observacao  = INPUT Clientes.Observacao.

  ASSIGN rSaved = ROWID(bCliente).

  /* pós-salvar: volta pro modo visualização e garante buffer AVAILABLE */
  RUN piHabilitaBotoesCli(TRUE).
  RUN piHabilitaCamposCli(FALSE).
  RUN piOpenQueryCli.
  REPOSITION qCli TO ROWID rSaved NO-ERROR.
  RUN piMostraCli.
END.


ON CHOOSE OF bt-canc DO:
  RUN piHabilitaBotoesCli(TRUE).
  RUN piHabilitaCamposCli(FALSE).
  RUN piMostraCli.
END.

ON CHOOSE OF btExport DO:
  DEFINE VARIABLE cCsv AS CHARACTER NO-UNDO.
  ASSIGN cCsv = "clientes.csv".
  OUTPUT TO VALUE(cCsv).
  PUT UNFORMATTED "CodCliente;NomCliente;Endereco;CodCidade;Observacao" SKIP.
  FOR EACH clientes NO-LOCK BY clientes.CodCliente:
    PUT UNFORMATTED
      STRING(clientes.CodCliente) ";"
      QUOTER(clientes.NomCliente) ";"
      QUOTER(clientes.CodEndereco) ";"
      STRING(clientes.CodCidade) ";"
      QUOTER(clientes.Observacao) SKIP.
  END.
  OUTPUT CLOSE.

  DEFINE VARIABLE cJson AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ja    AS JsonArray  NO-UNDO.
  DEFINE VARIABLE jo    AS JsonObject NO-UNDO.

  ASSIGN cJson = "clientes.json" ja = NEW JsonArray().
  FOR EACH clientes NO-LOCK BY clientes.CodCliente:
    jo = NEW JsonObject().
    jo:Add("CodCliente", clientes.CodCliente).
    jo:Add("NomCliente", clientes.NomCliente).
    jo:Add("Endereco",   clientes.CodEndereco).
    jo:Add("CodCidade",  clientes.CodCidade).
    jo:Add("Observacao", clientes.Observacao).
    ja:Add(jo).
  END.
  ja:WriteFile(cJson).
  DELETE OBJECT ja.

  OS-COMMAND SILENT VALUE("notepad " + cCsv).
  OS-COMMAND SILENT VALUE("notepad " + cJson).
END.


RUN piOpenQueryCli.
RUN piHabilitaCamposCli(FALSE).
RUN piMostraCli.

WAIT-FOR WINDOW-CLOSE OF FRAME fCli.

RETURN.


PROCEDURE piMostraCli:
  DEFINE BUFFER bCidade FOR cidades.

  IF AVAILABLE clientes THEN DO:
    FIND bCidade WHERE bCidade.CodCidade = clientes.CodCidade NO-LOCK NO-ERROR.
    IF AVAILABLE bCidade THEN
      ASSIGN vcCidade = STRING(bCidade.CodCidade) + " " + bCidade.NomCidade.
    ELSE
      ASSIGN vcCidade = "".

    DISPLAY Clientes.CodCliente Clientes.NomCliente Clientes.CodEndereco
            Clientes.CodCidade vcCidade Clientes.Observacao WITH FRAME fCli.
  END.
  ELSE
    CLEAR FRAME fCli.
END PROCEDURE.

PROCEDURE piOpenQueryCli:
  DEFINE VARIABLE rRecord AS ROWID NO-UNDO.

  IF AVAILABLE clientes THEN
    ASSIGN rRecord = ROWID(clientes).

  OPEN QUERY qCli FOR EACH clientes BY Clientes.CodCliente.
  IF rRecord <> ? THEN
    REPOSITION qCli TO ROWID rRecord NO-ERROR.
END PROCEDURE.

PROCEDURE piHabilitaBotoesCli:
  DEFINE INPUT PARAMETER pEnable AS LOGICAL NO-UNDO.
  DO WITH FRAME fCli:
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

PROCEDURE piHabilitaCamposCli:
  DEFINE INPUT PARAMETER pEnable AS LOGICAL NO-UNDO.
  DO WITH FRAME fCli:
    ASSIGN
      Clientes.CodCliente:SENSITIVE   = FALSE
      Clientes.NomCliente:SENSITIVE  = pEnable
      Clientes.CodEndereco:SENSITIVE = pEnable
      Clientes.CodCidade:SENSITIVE   = pEnable
      vcCidade:SENSITIVE              = FALSE
      Clientes.Observacao:SENSITIVE  = pEnable.
  END.
END PROCEDURE.

USING Progress.JSON.ObjectModel.*.


DEFINE TEMP-TABLE ttItem NO-UNDO
  FIELD CodItem       AS INTEGER
  FIELD CodProduto    AS INTEGER
  FIELD NomProduto    AS CHARACTER
  FIELD NumQuantidade AS INTEGER
  FIELD ValUnit       AS DECIMAL
  FIELD ValTotal      AS DECIMAL
  INDEX idx IS PRIMARY CodItem.

DEFINE QUERY qPed     FOR pedidos SCROLLING.
DEFINE QUERY qItensTT FOR ttItem  SCROLLING.

DEFINE VARIABLE cAction     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcCliente   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE vcEndereco  AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE vcCodCidade AS INTEGER   NO-UNDO.
DEFINE VARIABLE vcCidade    AS CHARACTER FORMAT "x(30)" NO-UNDO.

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

DEFINE BUTTON bt-addItem LABEL "Adicionar".
DEFINE BUTTON bt-modItem LABEL "Modificar".
DEFINE BUTTON bt-delItem LABEL "Eliminar".

DEFINE BROWSE brItens QUERY qItensTT
  DISPLAY
    ttItem.CodItem       LABEL "Item"
    ttItem.CodProduto    LABEL "Codigo"
    ttItem.NomProduto    LABEL "Produto"
    ttItem.NumQuantidade LABEL "Quantidade"
    ttItem.ValUnit       LABEL "Valor"
    ttItem.ValTotal      LABEL "Total"
  WITH SIZE 120 BY 8.

DEFINE FRAME fPed
  bt-pri AT 10
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

  pedidos.CodPedido     LABEL "Pedido"       COLON 20
  pedidos.DatPedido     LABEL "Data"         COLON 20
  pedidos.CodCliente    LABEL "Cliente"      COLON 20 vcCliente NO-LABEL
  vcEndereco            LABEL "Endereco"     COLON 20
  vcCodCidade           LABEL "Cidade"       COLON 20 vcCidade  NO-LABEL
  pedidos.Observacao    LABEL "Observacao"   COLON 20 SKIP(1)

  brItens SKIP(1)
  bt-addItem bt-modItem bt-delItem

  WITH SIDE-LABELS THREE-D SIZE 140 BY 25
       VIEW-AS DIALOG-BOX TITLE "Pedidos".


PROCEDURE piOpenQueryPed:
  DEFINE VARIABLE rRecord AS ROWID NO-UNDO.
  IF AVAILABLE pedidos THEN rRecord = ROWID(pedidos).
  OPEN QUERY qPed FOR EACH pedidos BY pedidos.CodPedido.
  IF rRecord <> ? THEN REPOSITION qPed TO ROWID rRecord NO-ERROR.
END PROCEDURE.

PROCEDURE piRefreshBrowseItens:
  DEFINE VARIABLE hQ AS HANDLE NO-UNDO.

  IF QUERY qItensTT:IS-OPEN THEN
    CLOSE QUERY qItensTT.

  OPEN QUERY qItensTT FOR EACH ttItem BY ttItem.CodItem.

  hQ = QUERY qItensTT:HANDLE.
  hQ:GET-FIRST() NO-ERROR.

  IF VALID-HANDLE(BROWSE brItens:HANDLE) THEN
    BROWSE brItens:REFRESH() NO-ERROR.
END PROCEDURE.


PROCEDURE piCarregaItensTT:
  DEFINE BUFFER bItens FOR itens.
  DEFINE BUFFER bProd  FOR produtos.

  EMPTY TEMP-TABLE ttItem.
  IF AVAILABLE pedidos THEN DO:
    FOR EACH bItens WHERE bItens.CodPedido = pedidos.CodPedido NO-LOCK BY bItens.CodItem:
      CREATE ttItem.
      ASSIGN
        ttItem.CodItem       = bItens.CodItem
        ttItem.CodProduto    = bItens.CodProduto
        ttItem.NumQuantidade = bItens.NumQuantidade
        ttItem.ValTotal      = bItens.ValTotal.

      FIND bProd WHERE bProd.CodProduto = bItens.CodProduto NO-LOCK NO-ERROR.
      IF AVAILABLE bProd THEN
        ASSIGN ttItem.NomProduto = bProd.NomProduto
               ttItem.ValUnit    = bProd.ValProduto.
    END.
  END.

  RUN piRefreshBrowseItens.
END PROCEDURE.

PROCEDURE piMostraPedido:
  DEFINE BUFFER bCliente FOR clientes.
  DEFINE BUFFER bCidade  FOR cidades.

  IF AVAILABLE pedidos THEN DO:
    FIND bCliente WHERE bCliente.CodCliente = pedidos.CodCliente NO-LOCK NO-ERROR.
    IF AVAILABLE bCliente THEN
      ASSIGN vcCliente   = bCliente.NomCliente
             vcEndereco  = bCliente.CodEndereco
             vcCodCidade = bCliente.CodCidade.
    ELSE ASSIGN vcCliente = "" vcEndereco = "" vcCodCidade = 0.

    FIND bCidade WHERE bCidade.CodCidade = vcCodCidade NO-LOCK NO-ERROR.
    ASSIGN vcCidade = (IF AVAILABLE bCidade THEN bCidade.NomCidade ELSE "").

    DISPLAY pedidos.CodPedido pedidos.DatPedido pedidos.CodCliente
            vcCliente vcEndereco vcCodCidade vcCidade pedidos.Observacao
            WITH FRAME fPed.

    RUN piCarregaItensTT.
  END.
  ELSE DO:
    CLEAR FRAME fPed.
    EMPTY TEMP-TABLE ttItem.
    RUN piRefreshBrowseItens.
  END.
END PROCEDURE.

PROCEDURE piHabilitaCamposPed:
  DEFINE INPUT PARAMETER pEnable AS LOGICAL NO-UNDO.
  DO WITH FRAME fPed:
    ASSIGN
      pedidos.CodPedido:SENSITIVE  = FALSE
      pedidos.DatPedido:SENSITIVE  = pEnable
      pedidos.CodCliente:SENSITIVE = pEnable
      pedidos.Observacao:SENSITIVE = pEnable.
  END.
END PROCEDURE.

PROCEDURE piHabilitaBotoesPed:
  DEFINE INPUT PARAMETER pEnable AS LOGICAL NO-UNDO.
  DO WITH FRAME fPed:
    ASSIGN
      bt-pri:SENSITIVE     = pEnable
      bt-ant:SENSITIVE     = pEnable
      bt-prox:SENSITIVE    = pEnable
      bt-ult:SENSITIVE     = pEnable
      bt-add:SENSITIVE     = pEnable
      bt-mod:SENSITIVE     = pEnable
      bt-del:SENSITIVE     = pEnable
      bt-save:SENSITIVE    = NOT pEnable
      bt-canc:SENSITIVE    = NOT pEnable
      bt-sair:SENSITIVE    = pEnable
      bt-addItem:SENSITIVE = TRUE
      bt-modItem:SENSITIVE = TRUE
      bt-delItem:SENSITIVE = TRUE.
  END.
END PROCEDURE.


ON ITERATION-CHANGED OF brItens DO:
  IF AVAILABLE ttItem THEN DO:
    ASSIGN
      bt-modItem:SENSITIVE IN FRAME fPed = TRUE
      bt-delItem:SENSITIVE IN FRAME fPed = TRUE.
  END.
  ELSE DO:
    ASSIGN
      bt-modItem:SENSITIVE IN FRAME fPed = FALSE
      bt-delItem:SENSITIVE IN FRAME fPed = FALSE.
  END.
END.


ON LEAVE OF pedidos.CodCliente IN FRAME fPed DO:
  DEFINE BUFFER bCli FOR clientes.
  DEFINE BUFFER bCid FOR cidades.
  DEFINE VARIABLE iCli AS INTEGER NO-UNDO.

  ASSIGN iCli = INTEGER(pedidos.CodCliente:SCREEN-VALUE IN FRAME fPed) NO-ERROR.
  IF ERROR-STATUS:ERROR OR iCli <= 0 THEN DO:
    MESSAGE "Informe um codigo de cliente valido." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  FIND bCli WHERE bCli.CodCliente = iCli NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bCli THEN DO:
    MESSAGE "Cliente " iCli " nao encontrado." VIEW-AS ALERT-BOX ERROR.
    ASSIGN vcCliente = "" vcEndereco = "" vcCodCidade = 0 vcCidade = "".
    DISPLAY vcCliente vcEndereco vcCodCidade vcCidade WITH FRAME fPed.
    RETURN NO-APPLY.
  END.

  ASSIGN
    vcCliente   = bCli.NomCliente
    vcEndereco  = bCli.CodEndereco
    vcCodCidade = bCli.CodCidade.

  FIND bCid WHERE bCid.CodCidade = vcCodCidade NO-LOCK NO-ERROR.
  ASSIGN vcCidade = (IF AVAILABLE bCid THEN bCid.NomCidade ELSE "").

  DISPLAY vcCliente vcEndereco vcCodCidade vcCidade WITH FRAME fPed.
END.


ON CHOOSE OF bt-add DO:
  ASSIGN cAction = "add".
  RUN piHabilitaBotoesPed(FALSE).
  RUN piHabilitaCamposPed(TRUE).

  EMPTY TEMP-TABLE ttItem.
  RUN piRefreshBrowseItens.

  CLEAR FRAME fPed.
  DISPLAY NEXT-VALUE(seqPedido) @ pedidos.CodPedido WITH FRAME fPed.
  DISPLAY TODAY @ pedidos.DatPedido WITH FRAME fPed.
END.

ON CHOOSE OF bt-mod DO:
  ASSIGN cAction = "mod".
  RUN piHabilitaBotoesPed(FALSE).
  RUN piHabilitaCamposPed(TRUE).
  RUN piMostraPedido.
END.

ON CHOOSE OF bt-save DO:
  DEFINE BUFFER bPedido FOR pedidos.
  DEFINE BUFFER bItens  FOR itens.

  DEFINE BUFFER bCli FOR clientes.
  DEFINE VARIABLE iCli AS INTEGER NO-UNDO.
  ASSIGN iCli = INPUT pedidos.CodCliente NO-ERROR.
  IF ERROR-STATUS:ERROR OR iCli <= 0 THEN DO:
    MESSAGE "Informe um codigo de cliente valido." VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.
  FIND bCli WHERE bCli.CodCliente = iCli NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bCli THEN DO:
    MESSAGE "Cliente " iCli " nao encontrado. Nao e possivel salvar."
      VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  IF cAction = "add" THEN DO:
    CREATE bPedido.
    ASSIGN bPedido.CodPedido = INPUT pedidos.CodPedido.
  END.
  ELSE DO:
    FIND bPedido WHERE bPedido.CodPedido = pedidos.CodPedido EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bPedido THEN DO:
      MESSAGE "Pedido nao encontrado para salvar." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
    END.
    FOR EACH bItens WHERE bItens.CodPedido = bPedido.CodPedido:
      DELETE bItens.
    END.
  END.

  ASSIGN
    bPedido.DatPedido  = INPUT pedidos.DatPedido
    bPedido.CodCliente = iCli
    bPedido.Observacao = INPUT pedidos.Observacao.

  DEFINE VARIABLE iItem      AS INTEGER NO-UNDO INITIAL 0.
  DEFINE VARIABLE vSomaTotal AS DECIMAL NO-UNDO INITIAL 0.

  FOR EACH ttItem BY ttItem.CodItem:
    ASSIGN
      iItem      = iItem + 1
      vSomaTotal = vSomaTotal + ttItem.ValTotal.

    CREATE bItens.
    ASSIGN
      bItens.CodPedido     = bPedido.CodPedido
      bItens.CodItem       = iItem
      bItens.CodProduto    = ttItem.CodProduto
      bItens.NumQuantidade = ttItem.NumQuantidade
      bItens.ValTotal      = ttItem.ValTotal.
  END.

  ASSIGN bPedido.ValPedido = vSomaTotal.

  RUN piHabilitaBotoesPed(TRUE).
  RUN piHabilitaCamposPed(FALSE).

  RUN piOpenQueryPed.
  DEFINE VARIABLE hQP AS HANDLE NO-UNDO.
  hQP = QUERY qPed:HANDLE.
  hQP:REPOSITION-TO-ROWID(ROWID(bPedido)) NO-ERROR.
  hQP:GET-NEXT() NO-ERROR.
  IF NOT AVAILABLE pedidos THEN hQP:GET-LAST() NO-ERROR.

  RUN piMostraPedido.       
END.

ON CHOOSE OF bt-canc DO:
  RUN piHabilitaBotoesPed(TRUE).
  RUN piHabilitaCamposPed(FALSE).
  RUN piMostraPedido.
END.


ON CHOOSE OF bt-addItem DO:
  DEFINE VARIABLE vCodProd AS INTEGER NO-UNDO.
  DEFINE VARIABLE vQtd     AS INTEGER NO-UNDO.
  DEFINE VARIABLE vValUnit AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vValTot  AS DECIMAL NO-UNDO.

  RUN TelaItem.p ("add", INPUT-OUTPUT vCodProd, INPUT-OUTPUT vQtd,
                  OUTPUT vValUnit, OUTPUT vValTot).

  IF RETURN-VALUE = "OK" THEN DO:
    DEFINE BUFFER bTT FOR ttItem.
    DEFINE VARIABLE nextItem AS INTEGER NO-UNDO INITIAL 1.

    FIND LAST bTT USE-INDEX idx NO-LOCK NO-ERROR.
    IF AVAILABLE bTT THEN nextItem = bTT.CodItem + 1.

    CREATE ttItem.
    ASSIGN
      ttItem.CodItem       = nextItem
      ttItem.CodProduto    = vCodProd
      ttItem.NumQuantidade = vQtd
      ttItem.ValUnit       = vValUnit
      ttItem.ValTotal      = vValTot.

    FIND FIRST produtos WHERE produtos.CodProduto = vCodProd NO-LOCK NO-ERROR.
    IF AVAILABLE produtos THEN ttItem.NomProduto = produtos.NomProduto.

    RUN piRefreshBrowseItens.
  END.
END.

ON CHOOSE OF bt-modItem DO:
  IF NOT AVAILABLE ttItem THEN RETURN NO-APPLY.

  DEFINE VARIABLE vCodProd AS INTEGER NO-UNDO.
  DEFINE VARIABLE vQtd     AS INTEGER NO-UNDO.
  DEFINE VARIABLE vValUnit AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vValTot  AS DECIMAL NO-UNDO.

  ASSIGN
    vCodProd = ttItem.CodProduto
    vQtd     = ttItem.NumQuantidade.

  RUN TelaItem.p ("mod", INPUT-OUTPUT vCodProd, INPUT-OUTPUT vQtd,
                  OUTPUT vValUnit, OUTPUT vValTot).

  IF RETURN-VALUE = "OK" THEN DO:
    ASSIGN
      ttItem.CodProduto    = vCodProd
      ttItem.NumQuantidade = vQtd
      ttItem.ValUnit       = vValUnit
      ttItem.ValTotal      = vValTot.

    FIND FIRST produtos WHERE produtos.CodProduto = vCodProd NO-LOCK NO-ERROR.
    IF AVAILABLE produtos THEN ttItem.NomProduto = produtos.NomProduto.

    RUN piRefreshBrowseItens.
  END.
END.

ON CHOOSE OF bt-delItem DO:
  IF AVAILABLE ttItem THEN DO:
    DELETE ttItem.
    RUN piRefreshBrowseItens.
  END.
END.

ON CHOOSE OF btExport DO:
  DEFINE VARIABLE iPed  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cBase AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCsv  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cJson AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ja    AS JsonArray  NO-UNDO.
  DEFINE VARIABLE jo    AS JsonObject NO-UNDO.

  ASSIGN iPed  = (IF AVAILABLE pedidos THEN pedidos.CodPedido ELSE 0)
         cBase = "itens_pedido_" + STRING(iPed)
         cCsv  = cBase + ".csv"
         cJson = cBase + ".json".

  OUTPUT TO VALUE(cCsv).
  PUT UNFORMATTED "CodItem;CodProduto;NomProduto;Quantidade;ValUnit;ValTotal" SKIP.
  FOR EACH ttItem BY ttItem.CodItem:
    PUT UNFORMATTED
      STRING(ttItem.CodItem) ";"
      STRING(ttItem.CodProduto) ";"
      QUOTER(ttItem.NomProduto) ";"
      STRING(ttItem.NumQuantidade) ";"
      STRING(ttItem.ValUnit,  ">>>>>>9.99") ";"
      STRING(ttItem.ValTotal, ">>>>>>9.99") SKIP.
  END.
  OUTPUT CLOSE.

  ASSIGN ja = NEW JsonArray().
  FOR EACH ttItem BY ttItem.CodItem:
    jo = NEW JsonObject().
    jo:Add("CodItem",       ttItem.CodItem).
    jo:Add("CodProduto",    ttItem.CodProduto).
    jo:Add("NomProduto",    ttItem.NomProduto).
    jo:Add("NumQuantidade", ttItem.NumQuantidade).
    jo:Add("ValUnit",       ttItem.ValUnit).
    jo:Add("ValTotal",      ttItem.ValTotal).
    ja:Add(jo).
  END.
  ja:WriteFile(cJson).
  DELETE OBJECT ja.

  OS-COMMAND SILENT VALUE("notepad " + cCsv).
  OS-COMMAND SILENT VALUE("notepad " + cJson).
END.

RUN piOpenQueryPed.
RUN piHabilitaCamposPed(FALSE).
RUN piHabilitaBotoesPed(TRUE).
RUN piMostraPedido.

VIEW FRAME fPed.
WAIT-FOR WINDOW-CLOSE OF FRAME fPed.
RETURN.

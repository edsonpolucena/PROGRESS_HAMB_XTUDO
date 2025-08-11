
DEFINE VARIABLE iCodPed   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cArq      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTotPed   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cProdDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE dValUnit  AS DECIMAL   NO-UNDO.

DEFINE BUFFER bPed  FOR pedidos.
DEFINE BUFFER bCli  FOR clientes.
DEFINE BUFFER bCid  FOR cidades.
DEFINE BUFFER bItem FOR itens.
DEFINE BUFFER bProd FOR produtos.

UPDATE iCodPed LABEL "Informe o n§ do Pedido:"
  WITH FRAME fAsk VIEW-AS DIALOG-BOX TITLE "Relatorio de Pedido".

FIND bPed WHERE bPed.CodPedido = iCodPed NO-LOCK NO-ERROR.
IF NOT AVAILABLE bPed THEN DO:
  MESSAGE "Pedido" iCodPed "nao encontrado." VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

FIND bCli WHERE bCli.CodCliente = bPed.CodCliente NO-LOCK NO-ERROR.
IF AVAILABLE bCli THEN
  FIND bCid WHERE bCid.CodCidade = bCli.CodCidade NO-LOCK NO-ERROR.

ASSIGN cArq = "rel_pedido_" + STRING(iCodPed) + ".txt".
OUTPUT TO VALUE(cArq).

PUT UNFORMATTED
  "                               Pedido" SKIP
  "Pedido: " bPed.CodPedido FORMAT ">>>>9"
  FILL(" ", 30)
  "Data: "   bPed.DatPedido  FORMAT "99/99/9999" SKIP
  "Nome:   " (IF AVAILABLE bCli THEN STRING(bCli.CodCliente) + "-" + bCli.NomCliente ELSE "") SKIP
  "Endereco: " (IF AVAILABLE bCli THEN bCli.CodEndereco ELSE "").
IF AVAILABLE bCid THEN
  PUT UNFORMATTED " / " bCid.NomCidade.
PUT SKIP
  "Observacaoo: " bPed.Observacao SKIP
  SKIP
  " Item  Produto                      Quantidade    Valor        Total" SKIP
  " ----- ---------------------------- ----------  ---------   ---------" SKIP.

ASSIGN vTotPed = 0.

FOR EACH bItem WHERE bItem.CodPedido = bPed.CodPedido NO-LOCK BY bItem.CodItem:
  FIND bProd WHERE bProd.CodProduto = bItem.CodProduto NO-LOCK NO-ERROR.

  ASSIGN
    cProdDesc = IF AVAILABLE bProd
                THEN STRING(bProd.CodProduto) + "-" + bProd.NomProduto
                ELSE STRING(bItem.CodProduto)
    dValUnit  = IF AVAILABLE bProd THEN bProd.ValProduto ELSE 0.

  PUT
    bItem.CodItem       FORMAT ">>>>9"   SPACE
    cProdDesc           FORMAT "x(28)"   SPACE
    bItem.NumQuantidade FORMAT ">>>>9"   SPACE
    dValUnit            FORMAT ">>>>>>9.99" SPACE(2)
    bItem.ValTotal      FORMAT ">>>>>>9.99"
    SKIP.

  ASSIGN vTotPed = vTotPed + bItem.ValTotal.
END.

PUT UNFORMATTED
  SKIP FILL("-", 75) SKIP
  FILL(" ", 50) "Total Pedido = " vTotPed FORMAT ">>>>>>9.99" SKIP.

OUTPUT CLOSE.

OS-COMMAND SILENT VALUE("notepad " + cArq).

RETURN.

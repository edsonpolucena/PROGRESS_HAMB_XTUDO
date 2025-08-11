
DEFINE VARIABLE cArq    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCidade AS CHARACTER NO-UNDO.

ASSIGN cArq = "rel_clientes.txt".

DEFINE BUFFER bCli FOR clientes.
DEFINE BUFFER bCid FOR cidades.

OUTPUT TO VALUE(cArq).

PUT UNFORMATTED
  "                         Relatorio de Clientes" SKIP
  FILL("-", 75) SKIP
  "Codigo  Nome                    Endereco                  Cidade           Observacao" SKIP
  "------  -----------------------  ------------------------  ---------------  ------------------------------" SKIP.

FOR EACH bCli NO-LOCK BY bCli.CodCliente:
  FIND bCid WHERE bCid.CodCidade = bCli.CodCidade NO-LOCK NO-ERROR.

  ASSIGN cCidade =
    IF AVAILABLE bCid
    THEN STRING(bCid.CodCidade) + "-" + bCid.NomCidade
    ELSE "".

  PUT
    bCli.CodCliente  FORMAT ">>>>9"    SPACE
    bCli.NomCliente  FORMAT "x(23)"    SPACE
    bCli.CodEndereco FORMAT "x(24)"    SPACE
    cCidade          FORMAT "x(15)"    SPACE
    bCli.Observacao  FORMAT "x(30)"
    SKIP.
END.

OUTPUT CLOSE.

OS-COMMAND SILENT VALUE("notepad " + cArq).

RETURN.

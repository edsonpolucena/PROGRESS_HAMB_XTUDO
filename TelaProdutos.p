USING Progress.JSON.ObjectModel.*.

DEFINE QUERY qProd FOR produtos SCROLLING.

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

DEFINE FRAME fProd
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

    Produtos.CodProduto COLON 20
    Produtos.NomProduto COLON 20
    Produtos.ValProduto     COLON 20

    WITH SIDE-LABELS THREE-D SIZE 140 BY 15
         VIEW-AS DIALOG-BOX TITLE "Produtos".

ENABLE ALL WITH FRAME fProd.

ON CHOOSE OF bt-pri  DO: GET FIRST qProd. RUN piMostraProd. END.
ON CHOOSE OF bt-ant  DO: GET PREV  qProd. RUN piMostraProd. END.
ON CHOOSE OF bt-prox DO: GET NEXT  qProd. RUN piMostraProd. END.
ON CHOOSE OF bt-ult  DO: GET LAST  qProd. RUN piMostraProd. END.

ON CHOOSE OF bt-add DO:
  ASSIGN cAction = "add".
  RUN piHabilitaBotoesProd(FALSE).
  RUN piHabilitaCamposProd(TRUE).

  CLEAR FRAME fProd.
  DISPLAY NEXT-VALUE(seqProduto) @ Produtos.CodProduto WITH FRAME fProd.
END.

ON CHOOSE OF bt-mod DO:
  ASSIGN cAction = "mod".
  RUN piHabilitaBotoesProd(FALSE).
  RUN piHabilitaCamposProd(TRUE).
  DISPLAY Produtos.CodProduto WITH FRAME fProd.
  RUN piMostraProd.
END.

ON CHOOSE OF bt-del DO:
  DEFINE VARIABLE lConf AS LOGICAL NO-UNDO.
  DEFINE BUFFER bProduto FOR produtos.
  

  MESSAGE "Confirma a exclusao da Produto " STRING(Produtos.CodProduto) "?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      TITLE "Excluir" UPDATE lConf.
  IF NOT lConf THEN 
    RETURN NO-APPLY.

  IF CAN-FIND(FIRST produtos
              WHERE Itens.CodProduto = Produtos.CodProduto NO-LOCK) THEN DO:
    MESSAGE "Não e possivel excluir: existem itens com produtos."
        VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.

  FIND bProduto
       WHERE bProduto.codProduto = Produtos.CodProduto
       EXCLUSIVE-LOCK NO-ERROR.

  IF AVAILABLE bProduto THEN DO:
    DELETE bProduto.

    RUN piOpenQueryProd.

    DO:
      GET FIRST qProd.
      IF AVAILABLE produtos THEN 
        RUN piMostraProd.
      ELSE DO:
        CLEAR FRAME fProd.
      END.
    END.
  END.
END.




ON CHOOSE OF bt-save DO:
    DEFINE BUFFER bProduto FOR produtos.
    
  IF cAction = "add" THEN DO:
    CREATE bProduto.
    ASSIGN bProduto.codProduto = INPUT Produtos.CodProduto.
  END.
  ELSE FIND bProduto
         WHERE bProduto.codproduto = Produtos.CodProduto
         EXCLUSIVE-LOCK NO-ERROR.

  ASSIGN
    bProduto.nomproduto = INPUT Produtos.NomProduto
    bProduto.ValProduto     = INPUT Produtos.ValProduto.

  RUN piHabilitaBotoesProd(TRUE).
  RUN piHabilitaCamposProd(FALSE).
  RUN piOpenQueryProd.
END.

ON CHOOSE OF bt-canc DO:
  RUN piHabilitaBotoesProd(TRUE).
  RUN piHabilitaCamposProd(FALSE).
  RUN piMostraProd.
END.

ON CHOOSE OF btExport DO:
  DEFINE VARIABLE cCsv AS CHARACTER NO-UNDO.
  ASSIGN cCsv = "produtos.csv".
  OUTPUT TO VALUE(cCsv).
  PUT UNFORMATTED "CodProduto;NomProduto;ValProduto" SKIP.
  FOR EACH produtos NO-LOCK BY produtos.CodProduto:
    PUT UNFORMATTED
      STRING(produtos.CodProduto) ";"
      QUOTER(produtos.NomProduto) ";"
      STRING(produtos.ValProduto, ">>>>>>9.99") SKIP.
  END.
  OUTPUT CLOSE.

  DEFINE VARIABLE cJson AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ja    AS JsonArray  NO-UNDO.
  DEFINE VARIABLE jo    AS JsonObject NO-UNDO.

  ASSIGN cJson = "produtos.json"  ja = NEW JsonArray().
  FOR EACH produtos NO-LOCK BY produtos.CodProduto:
    jo = NEW JsonObject().
    jo:Add("CodProduto", produtos.CodProduto).
    jo:Add("NomProduto", produtos.NomProduto).
    jo:Add("ValProduto", produtos.ValProduto).
    ja:Add(jo).
  END.
  ja:WriteFile(cJson).
  DELETE OBJECT ja.

  OS-COMMAND SILENT VALUE("notepad " + cCsv).
  OS-COMMAND SILENT VALUE("notepad " + cJson).
END.


RUN piOpenQueryProd.
RUN piHabilitaCamposProd(FALSE).
RUN piMostraProd.

WAIT-FOR WINDOW-CLOSE OF FRAME fProd.

RETURN.


PROCEDURE piMostraProd:
  IF AVAILABLE produtos THEN
    DISPLAY Produtos.CodProduto Produtos.NomProduto Produtos.ValProduto WITH FRAME fProd.
  ELSE
    CLEAR FRAME fProd.
END PROCEDURE.

PROCEDURE piOpenQueryProd:
    DEFINE VARIABLE rRecord AS ROWID       NO-UNDO.
    
    IF  AVAILABLE produtos THEN DO:
        ASSIGN rRecord = ROWID(produtos).
    END.
    
    OPEN QUERY qProd 
        FOR EACH Produtos BY Produtos.CodProduto. 
    IF rRecord <> ? THEN
    REPOSITION qProd TO ROWID rRecord NO-ERROR.
  
END PROCEDURE.


PROCEDURE piHabilitaBotoesProd:
  DEFINE INPUT PARAMETER pEnable AS LOGICAL NO-UNDO.
  DO WITH FRAME fProd:
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

PROCEDURE piHabilitaCamposProd:
  DEFINE INPUT PARAMETER pEnable AS LOGICAL NO-UNDO.
  DO WITH FRAME fProd:
    ASSIGN
      Produtos.CodProduto:SENSITIVE = FALSE
      Produtos.NomProduto:SENSITIVE = pEnable
      Produtos.ValProduto:SENSITIVE     = pEnable.
  END.
END PROCEDURE.

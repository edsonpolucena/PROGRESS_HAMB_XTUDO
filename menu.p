
CURRENT-WINDOW:WIDTH = 75.

DEFINE BUTTON btCid     LABEL "Cidades".
DEFINE BUTTON btProd    LABEL "Produtos".
DEFINE BUTTON btCli     LABEL "Clientes".
DEFINE BUTTON btPed     LABEL "Pedidos".
DEFINE BUTTON btRelCli  LABEL "Relatorios de Clientes".
DEFINE BUTTON btRelPed  LABEL "Relatorios de Pedidos".
DEFINE BUTTON btSair    LABEL "Sair" AUTO-ENDKEY.

DEFINE FRAME fMenu
    btCid    
    btProd    
    btCli     
    btPed
    btSair SPACE(3)SKIP(1)
    btRelCli  
    btRelPed  
      
    WITH SIDE-LABELS THREE-D SIZE 75 BY 5
         VIEW-AS DIALOG-BOX
         TITLE "Hamburgueria XTudo".

ENABLE ALL WITH FRAME fMenu.

ON CHOOSE OF btCid    DO: 
RUN TelaCidades.p.    
END.
ON CHOOSE OF btProd   DO:
RUN TelaProdutos.p.   
END.
ON CHOOSE OF btCli    DO:
RUN TelaClientes.p.  
END.
ON CHOOSE OF btPed    DO:
RUN TelaPedidos.p.   
END.
ON CHOOSE OF btRelCli DO:
RUN RelatorioClientes.p. 
END.
ON CHOOSE OF btRelPed DO:
RUN RelatorioPedidos.p.  
END.

WAIT-FOR
  CLOSE  OF FRAME fMenu.

RETURN.

    






# 📌 Sistema Hamburgueria XTudo

## 📖 Descrição
O **Hamburgueria XTudo** é um sistema desenvolvido em **Progress OpenEdge** para controle de clientes, produtos, pedidos e itens de uma hamburgueria.  
O sistema também conta com funcionalidades para emissão de relatórios e exportação de dados em **CSV** e **JSON**.

---

## 🎯 Funcionalidades Principais

- **Menu de acesso** para todas as funcionalidades do sistema.
- **Cadastro de Cidades**
  - Inclusão automática de código via sequência `seqCidade`.
  - Validação para não permitir exclusão se houver clientes vinculados.
- **Cadastro de Clientes**
  - Inclusão automática de código via sequência `seqCliente`.
  - Validação de cidade existente ao cadastrar/alterar cliente.
- **Cadastro de Produtos**
  - Inclusão automática de código via sequência `seqProduto`.
- **Cadastro de Pedidos e Itens**
  - Inclusão automática de código via sequência `seqPedido`.
- **Exportação de dados**
  - Botão **Exportar** gera arquivos `.JSON` e `.CSV`.
  - JSON contém pedidos e seus respectivos itens e valores.
- **Relatórios**
  - Relatório de dados cadastrais de clientes.
  - Relatório de pedidos por cliente, com itens, quantidades, valores e total.

---

## 🖥 Telas do Sistema

1. **Menu Principal** – Acesso rápido a todos os cadastros e relatórios.  
2. **Cadastro de Cidades** – Gerenciamento das cidades atendidas.  
3. **Cadastro de Produtos** – Cadastro e atualização dos produtos vendidos.  
4. **Cadastro de Clientes** – Registro e atualização dos clientes.  
5. **Cadastro de Pedidos e Itens** – Controle dos pedidos e seus itens.  
6. **Relatórios** – Geração de relatórios de clientes e pedidos.

---

## 🗄 Estrutura do Banco de Dados

O banco é criado no caminho `C:\treinamento\workspace\Projeto-Xtudo\xtudo.db` e possui as seguintes tabelas:

- **Cidades**
  - `CodCidade` (PK)
  - `NomCidade`
  - `CodUF`
- **Clientes**
  - `CodCliente` (PK)
  - `NomCliente`
  - `CodEndereco`
  - `CodCidade` (FK)
  - `Observacao`
- **Produtos**
  - `CodProduto` (PK)
  - `NomProduto`
  - `ValProduto`
- **Pedidos**
  - `CodPedido` (PK)
  - `CodCliente` (FK)
  - `DatPedido`
  - `ValPedido`
  - `Observacao`
- **Itens**
  - `CodPedido` (PK)
  - `CodItem` (PK)
  - `CodProduto` (FK)
  - `NumQuantidade`
  - `ValTotal`

---

## 📂 Estrutura de Arquivos do Projeto

- `/menu.p` → Tela principal do sistema  
- `/TelaCidades.p` → Cadastro de Cidades  
- `/TelaClientes.p` → Cadastro de Clientes  
- `/TelaProdutos.p` → Cadastro de Produtos  
- `/TelaPedidos.p` → Cadastro de Pedidos  
- `/TelaItem.p` → Cadastro de Itens do Pedido  
- `/RelatorioClientes.p` → Relatório de Clientes  
- `/RelatorioPedidos.p` → Relatório de Pedidos 

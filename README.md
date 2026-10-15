# Hasab

O **Hasab** é um projeto de compilador desenvolvido em Haskell, com o objetivo de traduzir um subconjunto da linguagem Haskell, voltado para expressões matemáticas e funções, para código MATLAB.

Atualmente, o projeto conta com a implementação da AST (*Abstract Syntax Tree*), do parser e de testes automatizados.

## Requisitos

- [GHC](https://www.haskell.org/ghcup/): compilador Haskell.
- [Cabal](https://www.haskell.org/cabal/): ferramenta para compilação e gerenciamento de dependências.
- GHCi (opcional): ambiente interativo para testar expressões e inspecionar a AST.

## Testes

Para executar a suíte de testes, rode o script na raiz do projeto:

```bash
./test.sh
```

No Windows, o script pode ser executado pelo Git Bash.

## Estrutura do projeto

- `app/`: ponto de entrada do programa.
- `src/AST.hs`: definição da árvore sintática abstrata.
- `src/Parser.hs`: implementação do parser.
- `test/`: testes automatizados.
- `test.sh`: script para execução dos testes.
- `Hasab.cabal`: configuração do projeto e suas dependências.

**Status:** projeto em desenvolvimento.

#!/usr/bin/env bash

set -e

cabal clean

echo "=================================="
echo " Compilando o projeto Hasab"
echo "=================================="

cabal build

echo ""
echo "=================================="
echo " Executando os testes"
echo "=================================="

cabal test

echo ""
echo "Todos os testes passaram!"

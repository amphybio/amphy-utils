# amphy-utils

Este é um repositório misto para colecionar funções utilitárias em diversas linguages: R, Python, C, Maple, etc.  Os códigos em Python e R podem ser instalados diretamente como pacotes chamados `amphy` (instruções abaixo).


## Pacote R

Instalação (da última versão lançada):

    > remotes::install_github('amphybio/amphy-utils', ref = remotes::github_release())

Uso:

    > amphy::function_example()

ou:

    > library(amphy)
    > function_example()


## Recomendações

Todos os membros do laboratório estão convidados a contribuir diretabente novas funções de uso geral, bem como corrigir bugs em funções existentes.

- **Se estiver interessado em adicionar funcionalidades a funções criadas por outra pessoa, abra um _issue_ para discussão ou faça um _pull request_.**

- Novos arquivos de código fonte devem ser adicionados ao subdiretório com nome correspondente à da linguagem (que precisa ser criado se não existir.  Exemplo: adicione código em Maple em `/maple/`.

4. Código em C/C++ deve ser incluído no pacote Python ou R se for usado exclusivamente através de API C/Cython/Rcpp.

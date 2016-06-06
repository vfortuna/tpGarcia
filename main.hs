{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, EmptyDataDecls, ViewPatterns #-}
module Main where
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Text (Text)
import Data.Time
import qualified Data.Text as T
import Control.Applicative
import Yesod
import Yesod.Form.Jquery
import Yesod.Static

data Pagina = Pagina{connPool :: ConnectionPool,
                     getStatic :: Static 
                    }

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Livro
   nome Text
   autor Text
   editora Text
   ano Day 
   tipo Text
   deriving Show

Aluno
   nome Text 
   cpf Int
   curso Text

Emprestimo
   livroId LivroId
   alunoId AlunoId
   data UTCTime default=now()
   processado Bool

|]

staticFiles "static"

mkYesod "Pagina" [parseRoutes| 
  /livro LivroR GET POST 
  /aluno AlunoR GET POST 
  /listalivro ListarLivroR GET
  /listaaluno ListarAlunoR GET
  /emprestimo EmprestimoR GET POST
  / HomeR GET
  /sucesso SucessoR GET
  /static StaticR Static getStatic
  /listaEmpreestimo ListarEmprestimoR GET
|]

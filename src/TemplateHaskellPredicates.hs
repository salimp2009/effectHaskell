--{-# LANGUAGE ConstraintKinds #-}
--{-# LANGUAGE EmptyCase #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE GADTs #-}
--{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeInType #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE DerivingStrategies #-}

module TemplateHaskellPredicates 
                    ( -- Shape(..)  ,
                     mkPredicates
                    ) 
                    where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | geometric shapes to be use in example
-- data Shape = Circle Double
--            | Square Double
--            | Triangle Double Double Double
           
-- | typical predicates that might be needed        
-- isCircle   :: Shape -> Bool
-- isSquare   :: Shape -> Bool
-- isTriangle :: Shape -> Bool        

-- | goal is to generate those using TH
-- the plan is;
-- Reify given data-type name and get info for data constructors
-- Use TH to generate predicate definitions
-- Use TH splices to generate code for predicates

-- >>>:t reify
-- reify :: Name -> Q Info

extractConstructors :: Info -> [Con]
extractConstructors (TyConI (DataD _ _ _ _ cons _)) = cons
extractConstructors _ = []

-- | although several data constructor 
-- this example implements only NormalC and ignores others
mkPredicate :: Con -> Q [Dec]
mkPredicate (NormalC name types) = 
  [d|
      $predicate = 
        \z -> case z of 
                $pat -> True
                _    -> False
  |]

  where
    predicate = varP $ mkName $ "is" <> nameBase name
    pat = conP name $ replicate (length types) wildP

mkPredicate _ = pure []  

{- ^
  "we build the predicate name using the mkName and nameBase functions. 
  The latter function removes all the qualifications, if present. 
  Then we build a pattern for the corresponding constructor. 
  This requires providing the right number of wildcards in
  the pattern."
-}

mkPredicates :: Name -> Q [Dec]
mkPredicates name = 
    reify name
    >>= fmap concat . mapM mkPredicate . extractConstructors
  

{-
  the built-in syntax 'f and ''T can be used to construct names, 
  The expression 'f gives a Name which refers to the value f currently in scope, 
  and ''T gives a Name which refers to the type T currently in scope. 
  These names can never be captured.
  lookupValueName and lookupTypeName are similar to 'f and ''T respectively, 
  but the Names are looked up at the point where the current splice is being run. 
  These names can never be captured.
  newName monadically generates a new name, which can never be captured.
  mkName generates a capturable name.

  Names constructed using newName and mkName may be used in bindings 
  (such as let x = ... or x -> ...), 
  but names constructed using lookupValueName, lookupTypeName, 'f, ''T may not.
-}




-- >>>:i nameBase
-- nameBase :: Name -> String
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i Name
-- type Name :: *
-- data Name = Name OccName NameFlavour
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i OccName
-- type OccName :: *
-- newtype OccName = OccName String
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i NameFlavour
-- type NameFlavour :: *
-- data NameFlavour
--   = NameS
--   | NameQ ModName
--   | NameU !Uniq
--   | NameL !Uniq
--   | NameG NameSpace PkgName ModName
--   	-- Defined in ‘Language.Haskell.TH.Syntax’


-- >>>:i conP
-- conP :: Quote m => Name -> [m Pat] -> m Pat
--   	-- Defined in ‘Language.Haskell.TH.Lib.Internal’

-- >>>:i Quote
-- type Quote :: (* -> *) -> Constraint
-- class Monad m => Quote m where
--   newName :: String -> m Name
--   {-# MINIMAL newName #-}
--   	-- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Quote Q -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Quote IO -- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i VarP
-- type Pat :: *
-- data Pat = ... | VarP Name | ...
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i Pat
-- type Pat :: *
-- data Pat
--   = LitP Lit
--   | VarP Name
--   | TupP [Pat]
--   | UnboxedTupP [Pat]
--   | UnboxedSumP Pat SumAlt SumArity
--   | ConP Name [Pat]
--   | InfixP Pat Name Pat
--   | UInfixP Pat Name Pat
--   | ParensP Pat
--   | TildeP Pat
--   | BangP Pat
--   | AsP Name Pat
--   | WildP
--   | RecP Name [FieldPat]
--   | ListP [Pat]
--   | SigP Pat Type
--   | ViewP Exp Pat
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i BangType
-- type BangType :: *
-- type BangType = (Bang, Type)
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i Type
-- type Type :: *
-- data Type
--   = ForallT [TyVarBndr Specificity] Cxt Type
--   | ForallVisT [TyVarBndr ()] Type
--   | AppT Type Type
--   | AppKindT Type Kind
--   | SigT Type Kind
--   | VarT Name
--   | ConT Name
--   | PromotedT Name
--   | InfixT Type Name Type
--   | UInfixT Type Name Type
--   | ParensT Type
--   | TupleT Int
--   | UnboxedTupleT Int
--   | UnboxedSumT SumArity
--   | ArrowT
--   | MulArrowT
--   | EqualityT
--   | ListT
--   | PromotedTupleT Int
--   | PromotedNilT
--   | PromotedConsT
--   | StarT
--   | ConstraintT
--   | LitT TyLit
--   | WildCardT
--   | ImplicitParamT String Type
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

 
-- >>>:i Con
-- type Con :: *
-- data Con
--   = NormalC Name [BangType]
--   | RecC Name [VarBangType]
--   | InfixC BangType Name BangType
--   | ForallC [TyVarBndr Specificity] Cxt Con
--   | GadtC [Name] [BangType] Type
--   | RecGadtC [Name] [VarBangType] Type
--   	-- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Eq Con -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Ord Con -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance Show Con -- Defined in ‘Language.Haskell.TH.Syntax’
-- instance [safe] Ppr Con -- Defined in ‘Language.Haskell.TH.Ppr’


-- >>>:i Info
-- type Info :: *
-- data Info
--   = ClassI Dec [InstanceDec]
--   | ClassOpI Name Type ParentName
--   | TyConI Dec
--   | FamilyI Dec [InstanceDec]
--   | PrimTyConI Name Arity Unlifted
--   | DataConI Name Type ParentName
--   | PatSynI Name PatSynType
--   | VarI Name Type (Maybe Dec)
--   | TyVarI Name Type
--   	-- Defined in ‘Language.Haskell.TH.Syntax’

-- >>>:i TyConI Dec
-- type Info :: *
-- data Info = ... | TyConI Dec | ...
--   	-- Defined in ‘Language.Haskell.TH.Syntax’
-- type Dec :: *
-- data Dec
--   = FunD Name [Clause]
--   | ValD Pat Body [Dec]
--   | DataD Cxt Name [TyVarBndr ()] (Maybe Kind) [Con] [DerivClause]
--   | NewtypeD Cxt Name [TyVarBndr ()] (Maybe Kind) Con [DerivClause]
--   | TySynD Name [TyVarBndr ()] Type
--   | ClassD Cxt Name [TyVarBndr ()] [FunDep] [Dec]
--   | InstanceD (Maybe Overlap) Cxt Type [Dec]
--   | SigD Name Type
--   | KiSigD Name Kind
--   | ForeignD Foreign
--   | InfixD Fixity Name
--   | PragmaD Pragma
--   | DataFamilyD Name [TyVarBndr ()] (Maybe Kind)
--   | DataInstD Cxt
--               (Maybe [TyVarBndr ()])
--               Type
--               (Maybe Kind)
--               [Con]
--               [DerivClause]
--   | NewtypeInstD Cxt
--                  (Maybe [TyVarBndr ()])
--                  Type
--                  (Maybe Kind)
--                  Con
--                  [DerivClause]
--   | TySynInstD TySynEqn
--   | OpenTypeFamilyD TypeFamilyHead
--   | ClosedTypeFamilyD TypeFamilyHead [TySynEqn]
--   | RoleAnnotD Name [Role]
--   | StandaloneDerivD (Maybe DerivStrategy) Cxt Type
--   | DefaultSigD Name Type
--   | PatSynD Name PatSynArgs PatSynDir Pat
--   | PatSynSigD Name PatSynType
--   | ImplicitParamBindD String Exp
--   	-- Defined in ‘Language.Haskell.TH.Syntax’



           



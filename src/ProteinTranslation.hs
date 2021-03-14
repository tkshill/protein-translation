module ProteinTranslation (proteins) where

import Data.List.Split
data ProteinCodon
  = AUG
  | UUU
  | UUC
  | UUA
  | UUG
  | UCU
  | UCC
  | UCA
  | UCG
  | UAU
  | UAC
  | UGU
  | UGC
  | UGG

data StopCodon
  = UAA
  | UAG
  | UGA

data Codon
  = Prot ProteinCodon
  | Stop StopCodon

data Protein
  = Methionine
  | Phenylalanine
  | Leucine
  | Serine
  | Tyrosine
  | Cysteine
  | Tryptophan
  deriving (Show)

toCodon :: String -> Maybe Codon
toCodon str =
  case str of
    "AUG" -> Just $ Prot AUG
    "UUU" -> Just $ Prot UUU
    "UUC" -> Just $ Prot UUC
    "UCU" -> Just $ Prot UCU
    "UUA" -> Just $ Prot UUA
    "UUG" -> Just $ Prot UUG
    "UCC" -> Just $ Prot UCC
    "UCA" -> Just $ Prot UCA
    "UCG" -> Just $ Prot UCG
    "UAU" -> Just $ Prot UAU
    "UAC" -> Just $ Prot UAC
    "UGU" -> Just $ Prot UGU
    "UGC" -> Just $ Prot UGC
    "UGG" -> Just $ Prot UGG
    "UAA" -> Just $ Stop UAA
    "UAG" -> Just $ Stop UAG
    "UGA" -> Just $ Stop UGA
    _ -> Nothing

toProtein :: ProteinCodon -> Protein
toProtein codon =
  case codon of
    AUG -> Methionine
    UUU -> Phenylalanine
    UUC -> Phenylalanine
    UUA -> Leucine
    UUG -> Leucine
    UCU -> Serine
    UCC -> Serine
    UCA -> Serine
    UCG -> Serine
    UAU -> Tyrosine
    UAC -> Tyrosine
    UGU -> Cysteine
    UGC -> Cysteine
    UGG -> Tryptophan

isProtCodon :: Codon -> Bool
isProtCodon (Prot _) = True
isProtCodon _ = False

toProtCodon :: Codon -> Maybe ProteinCodon
toProtCodon (Prot z) = Just z
toProtCodon _ = Nothing

tokenize :: String -> Maybe [Codon]
tokenize str = mapM toCodon $ chunksOf 3 str

proteins :: String -> Maybe [String]
proteins str =
  map (show . toProtein) <$> (castToProteinCodons . takeUntilStop =<< tokenize str)
  where
    takeUntilStop = takeWhile isProtCodon
    castToProteinCodons = mapM toProtCodon

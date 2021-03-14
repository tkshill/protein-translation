module ProteinTranslation (proteins) where

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

strToCodon :: String -> Maybe Codon
strToCodon str =
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

codonToProtein :: ProteinCodon -> Protein
codonToProtein codon =
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

parser :: String -> [Protein]
parser = parser_ []

parser_ :: [Protein] -> String -> [Protein]
parser_ accumulator (x : y : z : rest) =
  case strToCodon [x, y, z] of
    Nothing -> accumulator
    Just (Stop _) -> accumulator
    Just (Prot codon) ->
      flip parser_ rest $ accumulator ++ [codonToProtein codon]
parser_ accumulator _ = accumulator

proteins :: String -> Maybe [String]
proteins str =
  Just $ map show $ parser str

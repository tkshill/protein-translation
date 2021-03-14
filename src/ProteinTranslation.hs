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

parse :: String -> Maybe [Protein] -> Maybe [Protein]
parse (x : y : z : rest) acc@(Just ps) =
  continue =<< toCodon [x, y, z]
  where
    toPcodon (Prot c) = Just c
    toPcodon _ = Nothing
    updatedAcc pcodon = Just $ ps ++ [toProtein pcodon]
    recurse = parse rest . updatedAcc
    continue = maybe acc recurse . toPcodon

parse [] acc@(Just _) = acc
parse _ _ = Nothing

proteins :: String -> Maybe [String]
proteins str =
  fmap (map show) $ parse str $ Just []

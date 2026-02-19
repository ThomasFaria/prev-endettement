source("R/data/retrieval_webstat.R")
source("R/data/retrieval_insee.R")
source("R/data/retrieval_ecb.R")

# Une fois que vous avez créé votre fichier secrets.yaml, vous pouvez lire la clé API comme suit :
secrets <- yaml::read_yaml("secrets.yaml")

# Une liste des séries à récupérer :
series <- c(
        "CNFSI.Q.S.FR.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T", # Dette des ménages France en % du PIB
        "CNFSI.Q.N.DE.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
        "CNFSI.Q.N.IT.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
        "CNFSI.Q.N.ES.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
        "CNFSI.Q.N.I9.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
        "CNFSI.Q.N.GB.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
        "CNFSI.Q.N.US.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T"
    )

# Fonction pour récupérer les données :
data <- get_webstat(series_keys = series, api_key = secrets$api_key)

series <- c(
            "010567059", # Indice des prix des logements anciens - France métropolitaine - Ensemble - Base 100 en moyenne annuelle 2015 - Série CVS
            "010567013", # Indice des prix des logements anciens - Paris - Appartements - Base 100 en moyenne annuelle 2015 - Série CVS
            "010567079", # Indice des prix des logements anciens - Île-de-France : Ensemble - Base 100 en moyenne annuelle 2015 - Série CVS 
            "010567073" # Indice des prix des logements anciens - Province - Ensemble - Base 100 en moyenne annuelle 2015 - Série CVS
)
data_insee <- get_insee(series)


series_ecb <- c(
    "BLS.Q.FR.ALL.Z.H.H.B3.ZZ.D.FNET", # Demande de crédits à l'habitat (ménages)
    "BLS.Q.FR.ALL.Z.H.H.B3.ST.S.FNET", # Octroi de crédits à l'habitat (ménages)
    "BLS.Q.FR.ALL.SME.E.Z.B3.ZZ.D.FNET",	# Demande de crédit - PME
    "BLS.Q.FR.ALL.SME.E.Z.B3.ST.S.FNET",	# Octroi de crédits - PME
    "BLS.Q.FR.ALL.LE.E.Z.B3.ZZ.D.FNET",	# Demande de crédit - Grandes Entreprises
    "BLS.Q.FR.ALL.LE.E.Z.B3.ST.S.FNET"	# Octroi de crédits - Grandes Entreprises
)

data_ecb <- get_ecb(series_ecb)

# clean 2 - classify and map_module


# Load data ---------------------------------------------------------------


library(naturaList)
library(here)
library(raster)
source("function/bind_spec_df.R")

myrt.data <- read.csv(here::here("data", "myrteae_to_naturaList.csv"))
myrt.data.CC <- read.csv(here::here("data","myrteae_to_naturaList_CoordClean.csv"))
myrt.spec <- read.csv2(here::here("data","especialistas_Myrtaceae.csv"))

# Frequencia de nomes -----------------------------------------------------


freq.names <- get_det_names(myrt.data.CC, freq=T)

dez.pcento <- length(freq.names)*0.1
cinco.pcento <- dez.pcento/2
um.pcento <- dez.pcento/10


# Busca ativa 1 por cento mais frequentes ---------------------------------


freq.names[4481:length(freq.names)]

# especialistas presentes na lista previa (disponibilizada por V. Staggemeier)
pres.list.prev <- c("Sobral", "Faria",  "Holst",  "Landrum",
                    "Proença", "Kawasaki", "Legrand", "Rosa",
                    "Amorim", "Lughadha", "Lucas",
                    "Barroso", "Souza MAD", "Lima DF", "Santos")

chr.split1 <- strsplit(pres.list.prev, " ")

myrt.spec1.pcento <- myrt.spec[myrt.spec$LastName %in%  unlist(sapply(chr.split1, "[", 1)),]
myrt.spec1.pcento <- myrt.spec1.pcento[-c(11,19),]


absent.list <- c("L.F. Gonçalves", "F.R. Barrie", "A. Fuentes", " R. McVaugh",
                 "E.C.O.Chagas",
                 "Oliveira, AA de", "Guedes, ML")


# sem evidencias de ser especialista
# "L.F. Gonçalves"

# Se intitula especialista em eugenia                    
# https://www.missouribotanicalgarden.org/plant-science/plant-science/research-staff/article/372/barrie-fred-r.aspx 
# Eugenia (Myrtaceae)                 
F.R.Barrie <- "Fred R. Barrie" 

# taxonomista experiente - não especialista https://www.researchgate.net/profile/Alfredo_Fuentes4 - https://orcid.org/0000-0003-4848-4182
#A. Fuentes (LPB) 

# Experiente taxonomista, diversas publicações em Myrtaceae, autor de diversas espécies
# http://www.herbarium.unc.edu/Collectors/McVaugh.htm
R.McVaugh  <- "Rogers McVaugh" 

# é autor de espécies de myrtacae em co-autoria com JAMES LUCAS DA COSTA-LIMA 
# https://www.biotaxa.org/Phytotaxa/article/view/phytotaxa.373.3.4
# https://www.biotaxa.org/Phytotaxa/article/view/phytotaxa.408.2.6
# https://www.biotaxa.org/Phytotaxa/article/view/phytotaxa.399.1.4
E.C.O.Chagas <- "Earl Celestino de Oliveira Chagas" 

# Sem evidencias de ser especialista
# Oliveira, AA de
# Guedes, ML

new.spec.1.pcento <- c(F.R.Barrie, R.McVaugh, E.C.O.Chagas)

df.new.spec.1.pcento <- create_spec_df(new.spec.1.pcento)


myrt.spec1.pcento <- bind_spec_df(myrt.spec1.pcento, df.new.spec.1.pcento)


# Busca ativa 5 por cento mais frequentes ---------------------------------


freq.names[4300:4480]

pres.list.prev2 <- c("Holst", "Mazine", "Stadnik", "Sobral", "Lughadha", "Ibrahim",
                     "Giaretta", "Amorim", "Lucas", "Proença", "Barroso", "Santos",
                     "Lima DF", "Landrum", "Souza MC", "Molz", " Lourenço", "Faria", 
                     "Kawasaki", "Rosa", "Souza MAD", "Staggemeier","Calliari", 
                     "Lima L")



chr.split2 <- strsplit(pres.list.prev2, " ")

myrt.spec5.pcento <- myrt.spec[myrt.spec$LastName %in%  unlist(sapply(chr.split2, "[", 1)),]

absent.list2 <- c("Verdi, M", "Rotman, A. D.", "Romagnolo, M.B.", " B. M. T. Walter ",
                  "Korte, A", "G. G. Hatschbach ", " M.G. Caxambu", "Grizzon, M", 
                  "F.Barrie", "Gasper, AL de", "N. W. Snow", "Cordeiro, J", 
                  "Stival-Santos, A ", "Parra, Carlos", "O.T.Aguiar", "Vieira, R.S.",
                  "Mamede, MCH", "A. Pool", " Suemitsu, C.", "S. F. Smith", 
                  "Brotto, ML", "Soares-Silva, LH", "Silva, CAS da ", "Seubert, RC",
                  "Amshoff, J.H.", "Gomes, LA ", "Funez, L.A. ", "E. Barboza",
                  " W.D. Stevens", "Figueira, M.", "Ribeiro, C.L.",
                  "Schwirkowski, P.", "A. H. Liogier", "R. Liesner"," I. Bemerguy", 
                  "M. Nee", "Alverga, T.P.P.", "Geovane S. Siqueira ", 
                  "Tressens, S. G. ", "L.D. Meireles ", " Vanilde Citadini- Zanette ",
                  "L.C. Bernacci", " D. Villarroel ", "A. Pott", "Sánchez, Pablo E.", 
                  "Santana, JP ", " A. Fuentes ", "Ramos, JF", " Dreveck, S ", 
                  "Almeida Scabbia, RJ ", "Acevedo-Rodríguez, P.", "J. Mattos", 
                  "Selusniaki, M ", "Mendoza, Humberto", "Kassner-Filho, A.", 
                  "R. Vasquez","B.Schindler" ,"I. Loza")

absent.list2[1:10]
# não é especialista em Myrtaceae
# http://cncflora.jbrj.gov.br/portal/pt-br/equipe
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4251351Y9
# Marcio Verdi "Verdi, M"  


# especialista
## https://scholar.google.com.br/scholar?hl=pt-BR&as_sdt=0%2C5&as_vis=1&q=Alicia+D.+Rotman+botany+taxonomy&btnG=
A.D.Rotman <- "Alicia D. Rotman" 


# especialista
## http://www.pgb.uem.br/corpo-docente/mariza-barion-romagnolo
## http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4790049H1
M.B.Romagnolo <- "Mariza Barion Romagnolo"

#taxonomista experiente, não-especialista em myrtaceae
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4785397D6
# Bruno M. T. Walter 

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4246061H0
# Alexandre Korte

#taxonomista experiente, não-especialista em myrtaceae 
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4120483J7&tokenCaptchar=03AGdBq27odok97KJ0l7xpNIg8W6NhtRBCRmSqkRYTIYaB1NfdF54sPXbi26te3nQ_XErELTZSCQ3tyEFgs03GLSW3C1b7BT5ssbWG01mTYsxKag06Cbodd01camOBvHHG-bqxO7W3UzuZIqCxq5I45LZw0QQVhPfG-8OIkAzlEUwNzAWkXaC0QVnrIUcSOS5XiXpW1CnRMSM7IFIAgf0q6F9jP_cEHGamlOwWNEDynHCDiN55U2St7kNy3Iq5o7eWWa1ZdXF7BS6w-1REBgR5DjYd1BMge_VzYaa9cTUAQnyjdjqbn_EcmcjnYjOb8YcZ1RNwrvD6em_CuuntUfQf9ovkiIkWdf2kTvzPckEmu6Y9qkUzMH2R5oEfY3M18QepW7c7_oE8ir5mwUAcMENY17OYWZcyDEYmP8f5HUwJU95WefsMiQwR3k4coJ7idpQi5U8DiMnuinziUnCL7I902FKDjiLtY9Cv_w
# Gerdt Guenther Hatschbach

# sem evidencias de ser especialista
# M.G. Caxambu

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4680449Z6&tokenCaptchar=03AGdBq27XZs-QvpYaVGNRIvUyzFD31gM_TP4obszE8zBQNjE2OZE0VgaHE7AkIxnefKkn1KqNZtnvpy5Kqwc6EEJ1CgwM7wdqGUsAtIXhNivPPLEE1WYYVnMqjZT1ew1If8G5uEkb9DFZn2idX_8OnQiwjol0njBD9unf1cEqWruBgP186tvNN_2VMn-aMVNMqMJ3cr1A-14_ww8YcVfN1dHuV9mlbQtIAuiM0j_Zz4zQTXPOVUDamoGb7TIFNSOzDAakRBoh3Y-No1zgGqG4cdUEmToXDCpxwiE-wJ55YqfND3numIJH64FiASqXVBgbdkVdyA-xIFCL5EqFL4GVVChFEuOw2ZiES308FQ202jQy0L0kCgk4hO6Qf8ogJEG1QScH-LKVrv8ns3TgwUFo3RttEOWiT0sQMC7Dq49xRzfEqo07H0CMHA9m5KHqcVFKutu0GfW43WOsG4pi0RsLyYBXqefqJy0agg
# Marcos Grizzon  

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4717508Y1&tokenCaptchar=03AGdBq25sMdvB882SnztZgpx4EU3cdCYGjZFk7T-_lY7TORcejFEDRNT7eaU3xyI0E4uPYOP9rvRNnjFknscosWbq4NeU44FrEbrijv_SReKMNKD9ZTjRsW2hIy4Pq9uwjS0iRGnWBNRIhdZLyEozsEsDyiQOOveK0YP83L6bMqdP44l1pLsnlrryukOGW6VnYCinlk_OqjnzgYgZYecJ-GmlwkrQznmdCDGb99mVFAtG4zlgE-Rd1I-T2w_HZdNv0fAEJ9xi5ca0Jeg7I3redcI3N4TlwCqCyLhOxrzqvUpnX1DUE1zlUv1V7VXP8Ml98uNPU6ig9puZ4Z3jHEa0E_iITr4pJ2V2SBvQgGghTR2Pmugc6cuxmJ9_ocJFVAyl31-eyc94byOpMStKC5qOB4gpGYCzaCmVW7I8735NpwT-qAMoxAUc_pm7frsLOR7NH-3qbZsPxCwT
# Gasper, AL de

new.spec.5.pcento <- c(A.D.Rotman, M.B.Romagnolo)

absent.list2[11:20]
## especialista
# https://www.researchgate.net/profile/Neil_Snow
# https://scholar.google.com/citations?user=lRbhGKUAAAAJ&hl=en
N.W.Snow <- "Neil W. Snow"

# Não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4755385D5&tokenCaptchar=03AGdBq253VEPrMJBk66RztyjMMYwlBVf-091FRyw18KhIQugxQUVlOD_PsfAdI1IjoStPzGAnILcixkUH4moGatn70m3VX6isp36ZWX4hBOpbPod0t3cAhUY95oIX9xMCqJANFVg8I_HIxXUc2mqxcOeNtN3I_x0HK1jVFpvWGQ-ZDRnoNULTuMjX5dSDDRfMIPQM54lafutGz47H_r_1dc1wKv3qsRl6cQPkaswCN4FZV1xIEs-dprUQwfIxq31LXTZvPdmtnIAnX78Lg4qfrhZ2ep_vBgTjRRqdD2538i4QYdr7XGqC2KR2V__ZTY7ANm6YE32XYvZZk6f72yhknEuJIW1AbF4f3cD2lqupWEVrFtMtTi3kbPlsSXVa43bdkzCLaB3Bea7AiYgwxLVkaYYXCYSxOm5E_KRsMk0cwGJ7V0A9rkfzFdxx4lvgoz9mOSymqq9bg9_oegKBiQ2qS8Rnd5P6OEx6pw
# https://www.researchgate.net/profile/Juliano_Cordeiro2
# Cordeiro, Juliano 

# não-esecialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4238839E6&tokenCaptchar=03AGdBq24ei4mUScLXbbeoeMvbBeIiACGi5zPpv4f4k50LzWK2aoBLKUFekdJaypz7T_QigrkMsw4pqiLjeo17SZP_A7h07LYowi1AlkUHpMOAQYempnNk6_PqX3FexRZ04FKK6VZne5BfGEk1STovt-BiW_-zBxAWYXNXzaOTF5MACgCvB7HyfwXgnENlqsNjxRKqfZXDG0Qgr88reSQjEJ-8VJZbm2sXlwLmLxY0dyGyD9T6bZfkMqwkcw2WW38eI3DTrqgj5nUSK4mRKwijqcSIrrDknrZWquE1KMNxTCcbR6GRHYRbO0GQNPK42Kbne05m12_HjUfVgrquwgYxYbfiKiiqjSqEKE0amgixgCvJ96V8jdu-yn8KNlh2cGfzED2pa-SSE8Fp5vthStCrY2vQP24GeUH1wFDbZ-S553DhTA6NFXfAbgAXgYsY3Bwxbb-t3_jCEkh32uKm66p0jFUWivfphR1OWA
# Stival-Santos, Anita

# especialista
# https://plants.jstor.org/stable/10.5555/al.ap.person.bm000141525
# http://scienti.colciencias.gov.co:8081/cvlac/visualizador/generarCurriculoCv.do?cod_rh=0000063835
# https://www.ipni.org/a/38623-1
C.Parra <- c("Carlos Alberto Parra-O", 
             "Carlos Alberto Parra", 
             "Carlos Alberto Parra-Osorio")

# especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4788321T4&tokenCaptchar=03AGdBq25CFA1AQ-UF7-xKa9Nye9c2sU0sWIdByI4hcOHzMdbTaphge7Q3bBC3fjMTgpGaz2vhq3rn4gEx-1h5nFQL5GzmMH2cRVgHyqOBnWwjkNJSg-M9WdTskKfPPfPVwtfs81l0d4ZQChJlriW7VFwlmAQIu9Qcn8HN70b3L_Sf7OAVaFlqnnrz-IJnFVssrDA8D1pxowiV3bTKjH-eMTtPLw2E_H4fOwxTqzQ_RdYPiQRA--ILu0rlydpxKrkeibxdJWig35NqyZqNuMxWzGE6JwaVpFX5jk2lGKeV7GZ7Z8LRiLvF86dSgqcg7Fze1bZq1bAiVuTseJJ2ksbkpZMs7V96p591pPGLfaTvkayKK4lswwZQpdwamSYFTcagBQMtEGIs5wi2cxGg02auQoS1aJEFUuDcRstOxWI975bG1K_5k7fo3PxKJlx8oanqnP_iSQHBq6B0
O.T.Aguiar <- "Osny Tadeu de Aguiar"

# não-especialista
# sem evidencias de sere especialista
# "Vieira, R.S."

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4787714U1&tokenCaptchar=03AGdBq24Fl0Tj8vnTAFwWsITAQaK_H6RcU5YWWnp0JUtz48RWTQBJ10L6JeZPdAbQDH7AgH86YWQm79w10kdo-4oPUd-8uNK8pV729m-71Z1z2beh1AX2crGnknA2wsAozgczLaNK7lDbfn8MoU4rfud60nLSiVHNRqua-WvV9Po2CAhroXVfK53T-B3pg3IqIQljiyQW2GGmR5097_-5TLM2V8cZOyTvBVNVzdDqsXw8gLXD4Lg1XpzhCgtbr7YeApK66oVJZiJlHoAdsjY9kwhFWsqNI17smjD6Kyhb8nn57s9MvN2GKIdFONdAFAc1QR47UoL9bVklvZWCQy_lRItiHFxubQpgGk1Wr1YBZVxs3hFVZ3jOb3CVbSIrYqcjxSBKqK6gsbjbFYRI0RNJPhNgRimbHOdV1ToxQ5gSI1ekCLAdYHVBIJV-buAV0mp_mZsKi8T7Y3Z8IMR-qMiFg_plVGCLC0iw4A
# Maria Candida Henrique Mamede

# sem evidencia de ser especialista
# A. Pool

# Não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4786500Y4&tokenCaptchar=03AGdBq277FdFxaANacBaGUfNuH3vkk99rZanIMccq0FfBPdlE4_nR8zoRhvkjtRmdYuUB-UKCqO1lO3kgn0nhYqyPjE31msyi1zBBiGjzqbn7G5iBC6atStqeN6YTskn-cZYa0AiDPWUhGXOwcJ-H5I-iORmiEwsMqk_TEBylRiWEjcPj85TVw9v0Va4tEdSjEBv3L_GRnGw_sdPjtj0j9BaEobVMU1YSHQk3RPJsPftbfR1Zfpp9V7CGqpdFjIEF2sr6OK95HBykaE5IGp4UsnXmBWXW2NkJkWeT5Jsr-9IuXM1B9Y9qSWuCZcgLYBIrLZ5ZgPRAkuwoGS3BpDAIl39cyDXiixt5pHFzNnari8icz4ELWQB3jClSJR7nxGntWmImSKk0hlZXfhxQIq7nzMcqip60aWTAX8Q8EeNUAjLGIrpVoBmFDyTDhPR-rWnaEXjgqMFC8L8on9uMY-dHzQjk4IfAwN7mGQ
# Chieno Suemitsu

# sem evidencia de ser especialista
#S. F. Smith
new.spec.5.pcento <- c(new.spec.5.pcento, N.W.Snow, C.Parra, O.T.Aguiar)

absent.list2[21:30]

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4130224T3&tokenCaptchar=03AGdBq26VMCwrhhExK9nEK0ZuhuS2Mtd7RhHxlTltpLNDQQU5a6tzX67vrzzmzMsZuI2O_li2B2wuNJek3EbQMU4w-iHWigbTB10MzNAj35p2k_jXJ_jR6_JWA-dOAC6x_DbLZNL4ADWYQGSH3MKRPL-YvzrWSkEJt2ojhXuCleyyZb_K8yK15sjo9yWQGRnRqwQ3fIZreSZqxIw0Uue2ne6ybNoXjc9gNiGCVvp18tjFeidNzQg2JMB32VlBVt2s4NngQ6x0tZm2ef1nFoeYcAamJGO0Y1au7DjhMRy6NoiZ2wwgU-B6QV0FDP3dePey3Q17xGUz2df0mSZ-5sQ5cAjkhIyNgFerVORG99m5fMudaajVvqb5A8e5pFbf2JlySJWC2V7xbY_GSEsyvp0IvJJezTqchPSjNdAIH9QlhzXcayPPRVkaEVZic2prdjE-tejL_FuBHcmK
# Marcelo Leandro Brotto

# especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4785905P2
# Lucia Helena Soares e Silva
L.H.Soares.Silva <- c("Lucia Helena Soares e Silva", "Lucia Helena Soares-Silva")

# sem evidencia de ser especialista
#"Silva, CAS da "

#não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4383340H9&tokenCaptchar=03AGdBq26Y7yGz7_bGmbHzoF0-AMwdH68xpvlt9IMqQMEcE3KJC0b5bzXNHRQZxdkOl3KaFZmpGprNWT3OmiqdGdIFTSH9g20d03h7RRORvi8FVcHGdfvREvAL5NZwSzxz9URWVg6HVvBo3xmOScE9_hEounmmn7VwXKCcRLWCxDWxt9gBCGUN3w5zZVXXr4PUjBoxp5S-OFvu9xFWHDumqk_pDh5W-hp1k8WVMRkQJz3N03xAAnwFXbpr8024vL4LARGja0nJK5rdQ-5sGLx7_3zbKtinKEeuNpypJ_QqE9CK-A07Mq61VO3szaKUcZBJYUp-pst5kCKnLbxIjLsXtMBKu9b6PW2JvsjGu9yrEjfc9AJizD0saOqExqtgaRwW76m9c6qa-n0Ho9G86Gady0X4h4AEJEqiK6GH6ESEI4jWNFJzTxTcOkOii-VT32TzVX4_HIk39SKU
# Rafaela Cristina Seubert

# especialista
# citado recorentemente em descrições em myrtaceae
# https://www.biotaxa.org/Phytotaxa/article/view/phytotaxa.238.3.1
# https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1438-8677.1958.tb00605.x
G.J.H.Amshoff <- "G. J. H. Amshoff"

# sem evidencias de ser especialista
# "Gomes, LA "

#não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4249671P9&tokenCaptchar=03AGdBq25r_4YkTJPQ6loSTn6Jz58IRxbtL9JueJat5IhAChL4ugXvy5GuPKJ26rke6_qJRTlv3wYITD21cisfHqOOFIfEBHSqsCHQmp9c9VKYSWtrfGAQ7v12LoMKPtf3OcRLraiPYAMPHWYTPDqDo_Tr6V0I9gh0hFkLwzInXMf8Qe_xW8C_3RUG55TnMWc_5lirzbb__RwSAaHKymNCtwdWnaAynBmjTi19MSbLq1PfX7IHsUXZVjXsPxqrnKTkZUrMZSw8q6c_6CZJ_4kjLwghHI72UGhZaeet00z6QWA0ATndthaU1eblPS6zxfSLkqndP1B73LWyHJKGW547ZjYXlAK4xlUfxIgBKvo2DB4Nt8ehAel9ThmOLWYTX67q9AaiGhl1JJZW1ghlAUNdrRSqu4jp43jq_vFOzI2dOSOkNkn2TWPAh7oKxj0bopVyB7qhDrWFjMcJ
# Luís Adriano Funez


# não especialista
# duas opçõe para esse acronimo Eli Regina Barboza de Souza 
# e Gloria Estela Barboza, ambas não-especialistas
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4798506P5&tokenCaptchar=03AGdBq25ART8fhrEhkVxyu-XXPygXSFjuzto0luDRJqoY946fsnfRHWdO_Pdg6HO4F3MREOXmQnNPsmAb_wLCPiWwa3xP788NB0d4Cxc0SmgBFLFDb0fncCO1terixdNZ3zTpvW6f9Enpu1fmOk-peiJ9PGR556FBxqniTPLyPBd2p_2VFkfY-ygr3uZ2AobIh2SrwxnE5SlgUuLx1rDpVS6f5wrwKPRqhaoqvdoIVHDjbDC7xlKmYo-UaDzbKmNo4VOIMIVT9CWnRMGWyOkIMUlgPhL_Z18Jo-zYKPfkiuNSBHZuLEoAX17Kv_EID95Vb19EXt3vZnhg245gNro2DNCI4m7CkjfKTQ5ItIYu3LQse-0zOwvFd1J-uLjJ6vwESoS1VQtw3JfkpAZdvhdwVHP2Z75nHeh92YL6VWpeXo28SrUOBw5TkNDC09YqkrLDKzsboY0xrvs7
# https://www.researchgate.net/profile/Gloria_Barboza
# "E. Barboza"

# não-especialista
# https://www.missouribotanicalgarden.org/plant-science/plant-science/research-staff/article/488/stevens-w-d.aspx
# https://es.wikipedia.org/wiki/Warren_Douglas_Stevens
# https://www.ipni.org/a/27212-1
# Warren Douglas Stevens


new.spec.5.pcento <- c(new.spec.5.pcento, 
                       L.H.Soares.Silva,
                       G.J.H.Amshoff )

absent.list2[31:40]

#sem evidencias de ser especialista
# "Figueira, M."

# sem evidencias de ser especialista
# "Ribeiro, C.L."

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4301380U6&tokenCaptchar=03AGdBq26R6MXfTOkf_2ltvUImZCByjom637PZJk1OyVbd3NfoefQ-o7nTiLnR3xEt24EHglFQCB8mP87TKWTTQFbN-JUq-P9Ns6iBbGQF8PlZHoNYG-7gQzmt5lplTHh57gu5MtsVpzXOBTkIxI0J6P9UxHoOa_u5zTT1-bm8GV1J2jEtV5PDdtHJcG6F1e-ES_gEW3Q-DKfbySi4pLSgElsp6jmHvefkmqzdIoPltsQlvdh6ljEIUYLttlCXU-ep9MNhdVyc_4gUeaCQ-OgcWfrWYOQTZLoXSP4lVtCwnSbyKNZ_fCuvI3tDddxu8TuTT1NoUodLHTKGT8a7qyQl9rYdFFbaR06UlBBgACGM5ObpSa3RmnCRD3ISotM0YhogOYZb9uHWStWloMIaYRvA2EpcGCl_FUIplup_oHVxasBsF0DhpIguJHP23Elb6Eo02hwJNtkcdBFsVj2O6xQUQH1hdLOwH8nhqKHPZWfHL8obnTngTu66l0gCvRbpWe6m62XhUQHnPFNdbusz74I0gpIvQRMCtTX2jw
# Paulo Schwirkowski

# especialista
# https://www.ipni.org/a/14944-1
# https://www.jstor.org/stable/26600686?casa_token=-66tCyQmNDsAAAAA%3AeSHWELwcEyiAT-Xjv0Fdndq5PWd3GkNq70MrI3sktmLbtruECks5vjiXL-Kc7MR2BY4FdNuRrMTTNuZHuUG0xQQZ1iraqHDjnCAFqjA6um6WjLXwXak&seq=1#metadata_info_tab_contents
H.A.Liogier <- "Henri Alain Liogier"

# não-especialista 
# http://www.mobot.org/MOBOT/research/unseengarden/science1.shtml
# https://www.missouribotanicalgarden.org/plant-science/plant-science/research-staff/article/420/liesner-ronald.aspx
# Ronald Lee Liesner

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4768045D9&tokenCaptchar=03AGdBq25RX90rDe6CEOy7DY99gfsOJqsEuCKP8TJVJxXmf8D43xp7o9-pkJuNNngL7D7_8jVxW_eGgXCHeA7MwuvA3D1BXzALoQnhhH96o_aJZ-9y3ynkAJmG5ES7J3JVAtf1YnMi-GblFSp5z36MDoZSBE6-g0k_3ktfpz73XzcFS3a_aQHQnMMeLTMyoLVCvKN4u1c9iNo-G0bAY7yRlReA5D-52v92IVubr-iy8cRl5OiPBHT77ZkXCBT5knCJaZldTvwf7Ms317a8bUHQFefz_1zgvOVLRUSPLu7ULtOqYD4OTMD0eQOr_pfYbPNtQKqQiJnWepp44mM24uOQCxbuXcl6ic6iDgCIm-oN6NcE3ZRk9Z-2a1gdyvjrRCyeXxJLzaDvdY30AwsWpnSeh3XzzP1CsXRwmk0lqRcjPOitpm_MRy8Z4mxERZtHoFevn6jvnfy4JDwrgctFyGqNOw8Hj6R9QiI8jbWdCA3DYyb0EIdgArBeUuIFVzdPCacBnoypqcnVpautm-3yMCNl_eVlKz93_npg2A
# Ione Bemerguy

# não-especialista
# https://www.nybg.org/bsci/staf/nee.html
# Michael Nee

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4362888Y6&tokenCaptchar=03AGdBq244I1NBd15jeG94oxIXjQqO9WhRlmkdwSh13o4zhckTm6X-6A_jy79acBnbrwaZjBmk5eAUT7PmRtbVGYvSVeU9a_7wGnX1AakbmEyJUWjYfJgI0PxvLEaGWZ5VJo2FRKcmBr-w6MT-e_la46DbxuZcSJQkjevEy2kvLlen2SXmfJ-9E6p0bu-rzoU0IjcPF5C2-c9D5IklhT9V95ueBRaxXlbL2xK-ZZUSoa-6vkvJSCLWCBDZaC0zX1A11LMj3DaIw_Id5vPRek23X_IWBrMW5ZNn2-3TdMqSjImd_sPqQa9JhtQBIknV1HpoYf5pPScXPTzAvxMhyzcY76Kmtuet6N0YgfWPL2WC3DCjYqY_VHkJpY_przYZ7P-AfPCnfysmmZDEVyv1wCu2Nbj-_VJZhNY82tQZcSIupECtH7pt66rvAUtFg0fsFRvKZLZHheD2N19-8jdt2jrBQKbUR-Zajgkb5iysSpsvn_D_ltJrQiAtGw8gE-8pZAQtJqbnSytBkcKWFEhyxxTSHsfYdSowNMcdFg
# Thiago Palhares de Polari Alverga

# não-especialista
# http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4445383U1&tokenCaptchar=03AGdBq27AFnbO1Rg60ha0OYzjCC5gxuLohNQO0lDmcwGfaTeGpQ_ZacaY_uaWuQofTmz8ZtkMD_xJUCULUMFLNAK15w_l6G_HPTM77kzd1DoDmVS_nS2LLmBTQem9EBYfuWUkEOwgjenhnN4F9A2EDv1bTutrsGQ6pisrBDdxTDfJYXpS15tCH04HP3f1rc0MU0iZ_Xp7WQo_S978yHfMbCrc8d2npRCVqKvGyLik2kcUa3JzVfzN1mubKYY-Uo_lsk3_D02gzyWH2GzR28XhT8c_9cDWGsREuYjAWt6iiX6wR16NYBQhwy1_5Uq1FrH4ilopItoUQ4HFcZdRDhwGaq7A4BjdtxTh53wjwQYKedSsL9HimA70jVwpN3bnhHRWSP2WrH8OPXC92RBrBXlCTeDIp1pRSEBc9zExiA1Hc8wmtYbk2pBF5f-XYfZ-cnVASu65YHptl1sbwK5dMk5W704bCtclCKu4YXO87v1zSCTEUQRL3DvS8CV1o3bmi7h31NfZw271MPMVZRgm5GQIBZ-9ZVGnNnTuEw
# Geovane Souza Siqueira

# não-especialista
# https://es.wikipedia.org/wiki/Sara_Graciela_Tressens
# https://www.researchgate.net/profile/Sara_Tressens
# Sara Graciela Tressens

new.spec.5.pcento <- c(new.spec.5.pcento, H.A.Liogier)

absent.list2[41:50]

# especialista
# http://lattes.cnpq.br/6719386724662872
L.D.Meireles <- "Leonardo Dias Meireles"

# Não-especialista
# http://lattes.cnpq.br/7902320694662185
# Vanilde Citadini- Zanette 

# não-especialista
# http://lattes.cnpq.br/0405353122069365
# Luís Carlos Bernacci

# especialista
#  http://lattes.cnpq.br/6618471150517833
D.Villarroel <- c("Daniel Villarroel", 
                  "Daniel Villaroel",
                  "Daniel Villarroel Segarra")

# não-especialista
#  http://lattes.cnpq.br/8915975180559275
# Arnildo Pott

# sem evidencias de ser especialista
# Sánchez, Pablo E.

# sem evidencias de ser especialista
# Santana, JP

# taxonomista experiente - não especialista 
# https://www.researchgate.net/profile/Alfredo_Fuentes4 
# https://orcid.org/0000-0003-4848-4182
# A. Fuentes 

# não-especialista
# http://lattes.cnpq.br/3513243080084892
# José Ferreira Ramos

# não-especialista
# http://lattes.cnpq.br/5090248618258897
# Susana Dreveck


new.spec.5.pcento <- c(new.spec.5.pcento, D.Villarroel)


absent.list2[51:59]

# não-especialista
# http://lattes.cnpq.br/4734675977184527
# Almeida Scabbia, RJ

# não-especialista
# https://naturalhistory.si.edu/staff/pedro-acevedo
# Pedro Acevedo-Rodríguez


# especialista
# https://www.ipni.org/a/6244-1
J.R.Mattos <- "João Rodrigues Mattos"

# não-especialista
# http://lattes.cnpq.br/2910006382737918
# Marlon Alves Selusniaki

# não-especialista
# https://es.wikipedia.org/wiki/Humberto_Mendoza_(bot%C3%A1nico)
# "Mendoza, Humberto"

# sem evidencias de ser especialista
# Anderson Kassner Filho

# não-especialista
# https://www.researchgate.net/scientific-contributions/Roberto-Vasquez-13233862
# Roberto Vasquez

# sem evidencias de ser especialista
# B. Schindler

# não-especialista
# https://scholar.google.co.uk/citations?user=yHJXTakAAAAJ&hl=en
# Maria Isabel Loza Rivera "M. Isabel Loza"

new.spec.5.pcento <- c(new.spec.5.pcento, J.R.Mattos)

df.new.spec.5.pcento <- create_spec_df(new.spec.5.pcento)


myrt.spec5.pcento <- bind_spec_df(df.new.spec.5.pcento, myrt.spec5.pcento)
myrt.spec5.pcento <- unique.data.frame(bind_spec_df(myrt.spec5.pcento, myrt.spec1.pcento))

# Busca ativa 10 por cento mais frequentes ---------------------------------

freq.names[4074:4229]

pres.list.prev3 <- c("Tuler", "Staggemeier", "Kawasaki", 
                     "Sobral", "Landrum", "Hoslt", "Mazine",
                     "Lughadha", "Mazine-Capelo", "Faria", 
                     "Legrand", "Santos", " Lourenço"," Amorim", 
                     "Kawasaki", "Lima DF", "Lucas", 
                     "Giaretta", "Souza MC", "Santos", 
                     "Molz", "Oliveira MUI")



chr.split3 <- strsplit(pres.list.prev3, " ")

myrt.spec10.pcento <- myrt.spec[myrt.spec$LastName %in%  unlist(sapply(chr.split3, "[", 1)),]
myrt.spec10.pcento <- myrt.spec10.pcento[-c(9,21),]

absent.list3 <- c("Villarroel, D", "Vélez J.", "Uller, HF", "Silva, BD", 
                  "Keller, H. A. ", " K. Antunes", "J. Wesenberg", "Gasper, A.L.",
                  "Fonseca, ML", " Arantes, A.A.", "Silva, MA", "S. M. Silva", 
                  "Ronald Liesner", "Roldán F. ", "R.M.Camilo ", " C. L. Lundell", 
                  "S.F.Smith ", "Rogers McVaugh", "Peron, M.", " Landim, M", 
                  "Kondrat, H", "Köhler, M", "Farias, D.M. ", "D. E. Breedlove",
                  "F. S. Kawahara", "Cordeiro, J.", "A. Maruyama", "Urban, I.",
                  " R.P.Lyra-Lemos", "Pastório, FF ", "M.C.S.Mota", "Gissi, D.S.",
                  "Galvão, M.N", "F.R. Barrie", "Estevan, D.A.", "Tressens, S. G.", 
                  "Souza, V.C.", "Pena, MA", "P. Fiaschi ", " N.M.Ivanauskas ",
                  "L.Funch", "G. Jolochin", "esp, G. L. ", "E. V. E. J. Amaral",
                  "E. Jaramillo", "Stradmann, MTS", "Pezzi, E", "Matos, GMA ", 
                  "M.Figueira ",  " L.C. Bernacci", "James S. Miller ", 
                  "E. Freitas", " David H." , " Carvalho, PS ", 
                  " Bruno Machado Teles Walter", "U. Mehlig", "L. O. Santos",
                  "Souza, I.", "Soares Neto, R.L.","Loiola, M.I.B.", 
                  "Rafael Martins", "R.C. Mota", "R. Reis", "Mecenas, VV", 
                  "Macheda, D.R.", "M. Serrano ", "K.Coutinho ", "J. A. Kallunki", 
                  "E. M. Martínez S.", "D.Cardoso", "Cabrera, A. L. ", " A. S. Rosário",
                  "Völtz, R.R.", "Sampaio, V.S.", "Ramírez, J.G.", "N. T. da Silva", 
                  "Murray-Smith, C.", "Maruyama, A ", "L.F. Souza ", "L. Funch", 
                  "Jennings, L" , "J.L. Costa-Lima", "J.A.M.A. Gomes", "R.B. Torres", 
                  "J. R. Mattos", "J. P. Janovec", 
                  "Gil-Leguizamón, P.A.; Bravo-Pedraza, W.J.;
                  Morales-Puentes, M.E.; Zabala-Rivera, J.C.; 
                  Moreno-Gaona, D.A.; Gil-Novoa, J.E.; Manrique-Valderrama, N.T.;
                  Hernández-Ruiz, C.E.; Garzón-Peña, O.E.; Sánchez-Chavez, E.C.; 
                  Díaz-Pérez, C.N.; Alvarado-Fajardo, V.M.; Camargo-Espitia, N.A.;
                  Olaya-Angarita, J.A.; Hernández-Velandia, D.R.", " G. Jolochin", 
                  "Domingos A. Folli", "Carniello, MA", "  C.A. Mondin ", 
                  "Batalha, MA", "A. Mesquita", "B. M. T. Walter")

absent.list3[1:10]

# sem evidencias de ser especialista
# "Vélez J." 


# não especialista
# http://lattes.cnpq.br/2107798290775475
# Uller, HF


# sem evidencias de ser especialista
# Silva, BD

# nã-especialista
# http://ibone.unne.edu.ar/personal/25090bdb50185d81df8da660e58e9df0
# https://www.researchgate.net/profile/Hector_Keller
# Héctor Alejandro Keller 

# especialista
# http://lattes.cnpq.br/0440047976754166
K.Antunes <- "Kelly Antunes"


# não-especialista
# https://www.researchgate.net/profile/Jens_Wesenberg
# Jens Wesenberg

# sem evidencias de ser especialista
# Fonseca, ML
# "Silva, MA"

# não-especialista
# http://lattes.cnpq.br/5871222611463684
# Sandro Menezes Silva

# Não especialista
# http://scienti.colciencias.gov.co:8081/cvlac/visualizador/generarCurriculoCv.do?cod_rh=0000104280#articulos
# Francisco Javier Roldán Palacio 

# sem evidencias de ser especialista
#R.M.Camilo


# especialista
# https://www.ipni.org/a/5838-1
C.L.Lundell <- "Cyrus Longworth Lundell"

# sem evidencias de ser especialista
# S.F.Smith
# Peron, M.

# Especialista
# http://lattes.cnpq.br/3961388319501481
M.F.Landim <- "Myrna Friederichs Landim"

new.spec.10.pcento <- c(K.Antunes, C.L.Lundell, M.F.Landim)

absent.list3[21:30]

# Não-especialista
# http://lattes.cnpq.br/8598951987058576
# Hebert Kondrat

# não-especialista
# http://lattes.cnpq.br/1916820029502298
# Matias Köhler

# sem evidencias de ser especialista
# Farias, D.M.

# não-especialista
# https://www.ipni.org/a/1105-1
# Breedlove, Dennis E.

# sem evidencias de ser especialista
# F. S. Kawahara
# A. Maruyama

# antigo naturalista
# Identificaçõe possivelmente desatualizadas, portanto considerado
# não-especialista
# https://pt.wikipedia.org/wiki/Ignatz_Urban

# Não-especialista
# http://lattes.cnpq.br/3762389142515103
# Rosangela Pereira de Lyra Lemos

# não-especialista
# http://lattes.cnpq.br/4281382935357611
# Fábio Fiamoncini Pastório

absent.list3[31:40]

# Não-especialista
# http://lattes.cnpq.br/8458811696201959
# Maurício Carnaúba da Silva Mota

# não-especialista
# http://lattes.cnpq.br/7223497192057819  
# Danilo Soares Gissi

# não-especialista
# Marcelo Neto Galvão
# http://lattes.cnpq.br/0784714590561685

# não-especialista
# http://lattes.cnpq.br/8956072584800334
# Daniela Aparecida Estevan

# não-especialista
# http://lattes.cnpq.br/2457247935703929
# Vinicius Castro Souza

# sem evidencias de ser especialista
# Pena, MA

# não-especialista
# Pedro Fiaschi
# http://lattes.cnpq.br/5640945056555463

# não-especialista
# Natalia Macedo Ivanauskas
# http://lattes.cnpq.br/5822634990632546


absent.list3[41:50]

# especialista
# http://lattes.cnpq.br/8845087913178096
L.S.Funch <-  "Ligia Silveira Funch"

# sem evidencias de ser especialista
# Gabriela Jolochin
# E. V. E. J. Amaral
# "M.Figueira " 

new.spec.10.pcento <- c(new.spec.10.pcento, L.S.Funch)

absent.list3[51:60]

# não-especialista
# https://www.missouribotanicalgarden.org/plant-science/plant-science/resources/opportunities/undergraduate-studies/2018/2018-mentors-and-projects/articleid/361/james-s-miller-ph-d.aspx
# James S. Miller


# sem evidencias de ser especialista
# E. Freitas

# não-especialista
# https://scholar.google.com.br/citations?user=VNIHjC4AAAAJ&hl=pt-BR&oi=sra
# David H Lorence

# especialista
# http://lattes.cnpq.br/4108262957994798
P.S.Carvalho <- c("Plauto Simão de Carvalho",
                  "Plauto Simão de-Carvalho")


# não-especialista
# http://lattes.cnpq.br/5899434743926944
# Ulf Mehlig

# sem evidencias de ser especialista
# L. O. Santos
# Souza, I.


# não-especialista
# http://lattes.cnpq.br/3257097677118579
# Raimundo Luciano Soares Neto

# não-especialista
# http://lattes.cnpq.br/9609912655173252
# Maria Iracema Bezerra Loiola

new.spec.10.pcento <- c(new.spec.10.pcento, P.S.Carvalho)

absent.list3[61:70]

# não-especialista
# http://lattes.cnpq.br/8139143837827161
# Rafael Martins

# não-especialista
# http://lattes.cnpq.br/7258314080561387
# Rubens Custódio da Mota

# sem evidencias de ser especialista
# M. Serrano

# especialista
# http://lattes.cnpq.br/5784818033510157
K.Coutinho <- c("Karoline Coutinho de Santana", 
                "Karoline Coutinho")
                
# não-especialista
# http://lattes.cnpq.br/1728304852078603
# Jacquelyn Ann Kallunki


# sem evidencias de ser especialista
# "E. M. Martínez S."


# não-especialista
# http://lattes.cnpq.br/2228981567893077
# Domingos Benício Oliveira Silva Cardoso

new.spec.10.pcento <- c(new.spec.10.pcento, K.Coutinho)

absent.list3[71:80]

# não-especialista
# https://scholar.google.com.br/citations?user=aUIzVvIAAAAJ&hl=pt-BR&oi=sra
# Cabrera, Angel Lulio

# especialista
# http://lattes.cnpq.br/6278237027673165
A.L.Rosario <- "Alessandro Silva do Rosário"

# não-especialista
# http://lattes.cnpq.br/1270601568716170
# Rafael Rosenstock Völtz

# não-especialista
# http://lattes.cnpq.br/1691735885470558
# Valéria da Silva Sampaio

# sem evidencias de ser especialista
# Ramírez, J.G.
# N. T. da Silva

# não especialista
# https://scholar.google.com.br/scholar?q=Murray-Smith,+C.+taxonomy+botany&hl=pt-BR&as_sdt=0&as_vis=1&oi=scholart
# Murray-Smith, Charlotte


# sem evidencias de ser especialista
# "Maruyama, A "
# L.F. Souza 

new.spec.10.pcento <- c(new.spec.10.pcento, A.L.Rosario)

absent.list3[81:86]

# não-especialista
# http://sweetgum.nybg.org/science/ih/herbarium-details/?irn=126613
# Linda Jennings

# especialista]
# http://lattes.cnpq.br/2807168198577953
J.L.CostaLima <- c("James Lucas da Costa Lima", 
                   "James Lucas da Costa-Lima")

# não-especialista
# http://lattes.cnpq.br/6972753347501920
# José Ataliba Mantelli Aboin Gomes

# não-especialista
# http://lattes.cnpq.br/6996216192141104
# Roseli Buzanelli Torres

# não-especialista
# https://www.nybg.org/bsci/staf/janovec.html
# John P. Janovec

new.spec.10.pcento <- c(new.spec.10.pcento, J.L.CostaLima)

absent.list3[87]

# não-especialista
# https://scholar.google.com/citations?user=rPsm6cMAAAAJ&hl=tr
# Pablo Andres Gil Leguizamon

# não-especialista
# https://www.researchgate.net/profile/Maria_Morales-Puentes
# https://es.wikipedia.org/wiki/Mar%C3%ADa_Eugenia_Morales-Puentes
# Maria Eugenia Morales-Puentes

# não-especialista
# https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=Bravo-Pedraza%2C+William+J.&btnG=
# Bravo-Pedraza, William J.

# não-especialista
# https://scienti.minciencias.gov.co/cvlac/visualizador/generarCurriculoCv.do?cod_rh=0000315532#
#  	ZABALA RIVERA, JUAN CARLOS

# não-especialista
# https://www.researchgate.net/profile/Diego_Moreno_Gaona
# Moreno-Gaona, Diego Andres

# não-especialista
# https://scholar.google.com.br/citations?user=idOFO2kAAAAJ&hl=pt-BR&oi=sra
# Jorge Enrique Gil-Novoa

# não-especialista
# https://www.researchgate.net/profile/Naisla_Manrique_Valderrama
# Naisla T.  Manrique Valderrama

# sem evidencias de ser especialista 
# Hernández-Ruiz, C.E.
# Garzón-Peña, O.E.
# Sánchez-Chavez, E.C.

# não-especialista
# https://www.researchgate.net/profile/Carlos_Diaz25
# Carlos Nelson Diaz perez

# não-especialista
# https://www.researchgate.net/profile/Viviana_Alvarado-Fajardo
# Viviana Maritza Alvarado-Fajardo

# não-especialista
# https://www.researchgate.net/profile/Nohora_Camargo_Espitia
# Nohora Alba Camargo Espitia

# sem evidencias de ser especialista
# Olaya-Angarita, J.A.
# Hernández-Velandia, D.R.

absent.list3[88:94]

# sem evidencias de ser especialista
# "Domingos A. Folli"

# não-especialista
# http://lattes.cnpq.br/6659152924492446
# Maria Antonia Carniello

# não-especialista
# http://lattes.cnpq.br/1297010552679235
# Cláudio Augusto Mondin

# não-especialista
# http://lattes.cnpq.br/3893228815181339
# Marco Antônio Portugal Luttembarck Batalha

# sem evidencias de ser especialista
# A. Mesquita

new.spec.10.pcento
df.new.spec.10.pcento <- create_spec_df(new.spec.10.pcento)

myrt.spec10.pcento <- bind_spec_df(df.new.spec.10.pcento, myrt.spec10.pcento)
myrt.spec10.pcento <- unique.data.frame(bind_spec_df(myrt.spec5.pcento, myrt.spec10.pcento))


# Fim ---------------------------------------------------------------------

dim(myrt.spec1.pcento)


dim(myrt.spec5.pcento)

dim(myrt.spec10.pcento)


myrt.spec.all <- unique(bind_spec_df(myrt.spec, myrt.spec10.pcento))

dim(myrt.spec.all)


# Salvando arquivos -------------------------------------------------------

write.csv2(myrt.spec1.pcento, "data/especialistas_1porcento.csv", row.names = F)
write.csv2(myrt.spec5.pcento, "data/especialistas_5porcento.csv", row.names = F)
write.csv2(myrt.spec10.pcento, "data/especialistas_10porcento.csv", row.names = F)
write.csv2(myrt.spec.all, "data/especialistas_todos.csv", row.names = F)

not.name <- c("Anônimo",
              "?", 
              "Anónimo",
              "Foster's catalog note file", 
              "Sem determinador" ) 

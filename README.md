# STAT • DESC — ETL + Univarié + Bivarié + IA (Groq)

Application **Shiny** tout-en-un pour :
- **ETL / Prétraitement** (import CSV, gestion des NA, imputation KNN, winsorisation des extrêmes, conversion QT→QL),
- **Statistiques univariées** (tableaux & graphes automatiques),
- **Statistiques bivariées** (tableaux de contingence, corrélation de Pearson, V de Cramer, ANOVA, boxplots/nuages),
- **IA intégrée** (génération de rapport Markdown & chatbot) propulsée par **Groq** (`llama-3.1-8b-instant`).

> Démo déployée : *(ajoute l’URL shinyapps.io une fois publiée)*  
> Exemple : `https://<ton_compte>.shinyapps.io/app1_IA/`

---

## ⚡ Sommaire

- [Aperçu](#aperçu)
- [Fonctionnalités](#fonctionnalités)
- [Structure du projet](#structure-du-projet)
- [Prérequis](#prérequis)
- [Installation & exécution locale](#installation--exécution-locale)
- [Configuration des clés (Groq)](#configuration-des-clés-groq)
- [Déploiement (shinyappsio)](#déploiement-shinyappsio)
- [Jeu de données attendu](#jeu-de-données-attendu)
- [Paramétrages & bonnes pratiques](#paramétrages--bonnes-pratiques)
- [Dépannage (FAQ)](#dépannage-faq)
- [Contribuer](#contribuer)
- [Crédits & Licence](#crédits--licence)

---

## Aperçu

- **Onglet 1 — Préparation (ETL)**  
  Import CSV, gestion NA, imputation KNN, winsorisation, visualisation NA, export final.  

- **Onglet 2 — Univarié**  
  Tableau statistique, graphes automatiques (barres, histogramme, boxplot, etc.).  

- **Onglet 3 — Bivarié**  
  Tableaux de contingence, tests Chi2, corrélation, V de Cramer, ANOVA, graphes comparatifs.  

- **Onglet 4 — IA (Groq)**  
  Rapport Markdown structuré + chatbot intelligent propulsé par Groq.

---

## Fonctionnalités

- UI `shinydashboard` + widgets modernes
- Visualisation NA robuste avec `visdat`
- IA : génération de rapports structurés + chatbot contextuel
- Gestion des erreurs réseau/API
- Déploiement facile via `rsconnect` sur shinyapps.io

---

## Structure du projet

```
app1_IA/
├─ app.R
├─ renv.lock
├─ renv/
├─ .Rprofile
├─ .Renviron (local uniquement)
├─ .gitignore
├─ rsconnect/
└─ README.md
```

---

## Prérequis

- R ≥ 4.3  
- RStudio  
- `renv` pour gérer les dépendances  
- Clé API Groq (`GROQ_API_KEY`)  
- Compte shinyapps.io pour déploiement en ligne

---

## Installation & exécution locale

```r
# 1. Installer renv si nécessaire
install.packages("renv")

# 2. Restaurer l'environnement
renv::restore()

# 3. Lancer l'application
shiny::runApp("app.R")
```

---

## Configuration des clés (Groq)

Créer un fichier `.Renviron` à la racine du projet :

```
GROQ_API_KEY=sk_tacleici
```

Puis :

```r
Sys.getenv("GROQ_API_KEY")
# Doit renvoyer la clé
```

*(En production sur shinyapps.io, utiliser Environment Variables de préférence)*

---

## Déploiement (shinyapps.io)

```r
# 1. Installer rsconnect
install.packages("rsconnect")
library(rsconnect)

# 2. Configurer le compte
rsconnect::setAccountInfo(name   = "<ton_compte>",
                          token  = "<ton_token>",
                          secret = "<ton_secret>")

# 3. Déployer
rsconnect::deployApp()
```

URL obtenue :
```
https://<ton_compte>.shinyapps.io/app1_IA/
```

---

## Jeu de données attendu

- Fichier CSV
- Variables quantitatives numériques ou qualitatives
- Encodage UTF-8 recommandé
- Possibilité de convertir QT → QL

---

## Paramétrages & bonnes pratiques

- Winsorisation : 1%–99%
- NA : suppression ou imputation KNN
- IA : `llama-3.1-8b-instant`
- Sécurité : ne jamais versionner `.Renviron`
- Versionner uniquement `renv.lock` (équivalent `requirements.txt` en Python)

---

## Dépannage (FAQ)

- **Clé manquante** : vérifier `.Renviron` ou les variables shinyapps.io  
- **Erreur Groq API** : problème réseau ou clé expirée  
- **Limite shinyapps.io** : 25 h/mois → plan gratuit
- **Erreur packages** : `renv::snapshot()` puis redeployer

---

## Contribuer

1. Forker le repo  
2. Créer une branche `feature/...`  
3. Commit & push  
4. Ouvrir une Pull Request

---

## Crédits & Licence

- Auteur : **Idriss** — INSSEDS  
- Technologies : R, Shiny, Groq API  
- Licence : MIT (modifiable)

# 📊 STATISTIQUE_IA — Application Shiny IA & Statistique Avancée

> **Auteur : KARABOUE VAKABOU** — Statisticien, Data Scientist, Expert en Intelligence Artificielle

Cette application Shiny est une **plateforme analytique avancée** combinant **statistique descriptive**, **prétraitement intelligent**, et **intégration d’IA générative Groq** pour la production de rapports automatisés.  
Elle a été conçue pour offrir une **infrastructure reproductible, extensible et conforme aux meilleures pratiques internationales** en matière d’analyse statistique, de science des données et d’intelligence artificielle appliquée.

👉 **Accéder à l’application en ligne** : [https://kara9934.shinyapps.io/app1_ia/](https://kara9934.shinyapps.io/app1_ia/)

---

## ⚡ Sommaire

- [1. Contexte et objectifs](#1-contexte-et-objectifs)
- [2. Aperçu fonctionnel](#2-apercu-fonctionnel)
- [3. Architecture technique](#3-architecture-technique)
- [4. Stack et dépendances](#4-stack-et-dépendances)
- [5. Jeux de données attendus](#5-jeux-de-données-attendus)
- [6. Installation locale](#6-installation-locale)
- [7. Gestion des clés et sécurité](#7-gestion-des-clés-et-sécurité)
- [8. Déploiement](#8-déploiement)
- [9. Gouvernance, conformité & meilleures pratiques IA](#9-gouvernance-conformité--meilleures-pratiques-ia)
- [10. Dépannage / FAQ](#10-dépannage--faq)
- [11. Portée internationale & innovation](#11-portée-internationale--innovation)
- [12. Contribuer](#12-contribuer)
- [13. Crédits & Licence](#13-crédits--licence)

---

## 1. Contexte et objectifs

L’application **Statistique_IA** est une solution analytique intégrée conçue pour :

- 🧮 Réaliser des analyses statistiques automatisées et reproductibles.  
- 🤖 Exploiter l’IA générative Groq pour interpréter et communiquer les résultats.  
- 🧰 Réduire la complexité des pipelines analytiques tout en garantissant la traçabilité.  
- 🌍 Offrir une interface conviviale accessible aux analystes, chercheurs et décideurs.

Elle s’inscrit dans une vision **pluridisciplinaire**, alliant rigueur scientifique, excellence technologique et ouverture à l’innovation.

---

## 2. Aperçu fonctionnel

### 🧹 Prétraitement (ETL)
- Import de jeux de données CSV
- Nettoyage et transformation (QT → QL)
- Imputation intelligente des valeurs manquantes
- Winsorisation pour réduire l’impact des valeurs extrêmes

### 📈 Statistique descriptive
- **Univarié** : tableaux de fréquences, mesures de tendance, dispersion, histogrammes, boxplots…  
- **Bivarié** : corrélation Pearson, chi², V de Cramer, ANOVA, η², graphiques croisés…

### 🤖 Intelligence Artificielle
- Intégration directe de l’API Groq (modèle `llama-3.1-8b-instant`)
- Génération automatique de rapports narratifs
- Assistance IA contextuelle intégrée

---

## 3. Architecture technique

```
.
├── app.R                    # Code principal Shiny (UI + Server)
├── renv.lock                # Dépendances versionnées
├── renv/
│   ├── activate.R
│   └── settings.json
├── .gitignore               # Fichiers exclus du dépôt
└── README.md                # Documentation
```

🧠 **Principes clés de l’architecture** :
- Séparation claire UI / Server
- Gestion reproductible des dépendances avec `renv`
- Configuration sécurisée via variables d’environnement
- Déploiement cloud stateless sur shinyapps.io

---

## 4. Stack et dépendances

| Composant             | Description                                                |
|-------------------------|----------------------------------------------------------|
| 🖥️ **Langage**           | R 4.3.2                                                   |
| 🧰 **Framework**         | Shiny                                                     |
| 🧠 **IA**                | Groq API – `llama-3.1-8b-instant`                         |
| 📦 **Gestion dépendances** | renv                                                   |
| ☁️ **Hébergement**       | shinyapps.io                                             |

Dépendances principales :
- `shiny`, `dplyr`, `DT`, `plotly`, `ggplot2`, `httr`, `jsonlite`, `visdat`, `tidyr`, `forcats`, `DMwR2`

---

## 5. Jeux de données attendus

- Format : **CSV**, séparateur `,` ou `;`
- Données quantitatives et qualitatives
- Valeurs manquantes acceptées dans la limite de 30%
- Taille recommandée : < 50 Mo (limite shinyapps.io)

✅ Prévoir des **entêtes claires** et des variables bien typées pour des analyses optimales.

---

## 6. Installation locale

```bash
git clone https://github.com/kara99313/Statistique_IA.git
cd Statistique_IA
```

Créer un fichier `.Renviron` à la racine avec :
```
GROQ_API_KEY=votre_cle_groq_ici
```

Dans R :
```r
renv::restore()
shiny::runApp("app.R")
```

L’application s’ouvrira automatiquement dans votre navigateur 🌐.

---

## 7. Gestion des clés et sécurité

- 🔑 La clé API Groq n’est **jamais versionnée** (`.Renviron` exclu via `.gitignore`)  
- 🧰 Chaque utilisateur doit créer son propre `.Renviron` local  
- 🧠 Sur shinyapps.io, la clé est définie comme **variable d’environnement** dans la configuration

---

## 8. Déploiement

Déploiement simple :

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name="<user>", token="<token>", secret="<secret>")
rsconnect::deployApp()
```

Déploiement recommandé :
- Clé Groq définie côté serveur
- Application stateless
- Version reproductible via `renv.lock`

Lien en production : 👉 [https://kara9934.shinyapps.io/app1_ia/](https://kara9934.shinyapps.io/app1_ia/)

---

## 9. Gouvernance, conformité & meilleures pratiques IA

- 📜 Respect des bonnes pratiques en matière de gestion de modèles et de reproductibilité scientifique.  
- 🧾 Journalisation et traçabilité des analyses.  
- 🧠 Utilisation responsable de l’IA conformément aux cadres éthiques internationaux (OCDE, UE, RGPD).  
- 🔐 Confidentialité des données renforcée (aucune donnée sensible stockée).

---

## 10. Dépannage / FAQ

| Problème                              | Solution                                                            |
|----------------------------------------|---------------------------------------------------------------------|
| `Sys.getenv("GROQ_API_KEY")` vide      | Vérifier `.Renviron` puis redémarrer R                              |
| Packages manquants                     | `renv::restore()`                                                   |
| Déploiement échoué                     | Consulter les logs shinyapps.io                                     |
| API Groq non reconnue                  | Vérifier la validité de la clé et la connexion Internet              |
| Lenteur de chargement                  | Vérifier la taille du jeu de données et les ressources disponibles   |

---

## 11. Portée internationale & innovation

Ce projet se distingue par :

- 🌍 **Interopérabilité** : reproductible sur tout système supportant R  
- 🧠 **Innovation méthodologique** : combinaison ETL automatisé + IA générative appliquée à l’analyse statistique  
- 📊 **Ouverture scientifique** : structure extensible pour accueillir de nouveaux modules analytiques  
- 🧰 **Portabilité** : déploiement flexible (shinyapps.io, Docker, serveur privé)  
- 📡 **Impact socio-économique** : application dans la recherche, la finance, la santé et les politiques publiques.

---

## 12. Contribuer

Les contributions sont **ouvertes à la communauté** selon des standards professionnels.

### 🪜 Étapes
1. Forker le dépôt  
2. Créer une branche (`feature/...`)  
3. Commit et push des modifications  
4. Pull Request avec documentation claire

✅ Merci de ne pas inclure de **secrets**, et de documenter chaque ajout.

---

## 13. Crédits & Licence

- 📌 Auteur : **KARABOUE VAKABOU**  
  Statisticien | Data Scientist | Expert en IA
- 🏛️ Institution : INSSEDS  
- 📅 Dernière mise à jour : Octobre 2025  
- 📜 Licence : MIT — usage académique et non commercial recommandé.

---

## 📎 Liens utiles

- 📂 [Dépôt GitHub](https://github.com/kara99313/Statistique_IA)  
- 🌐 [Application Shiny déployée](https://kara9934.shinyapps.io/app1_ia/)  
- 🧠 [Documentation Groq](https://console.groq.com)  
- 📘 [Documentation Shiny](https://shiny.posit.co/)

---

# 🎓 Analyse des facteurs déterminants de la réussite en L1 Économie et Gestion (Sud-Ouest)

Ce projet vise à étudier les relations de dépendance entre plusieurs variables socio-économiques, scolaires et personnelles, et la réussite des étudiants en première année de Licence Économie et Gestion dans les universités du Sud-Ouest (Pau, Bordeaux, Toulouse). L’objectif est d’identifier les facteurs les plus influents pour modéliser la probabilité de réussite des futurs étudiants.

## 🧠 Objectifs
- Comprendre les déterminants clés de la réussite en L1 Économie-Gestion.  
- Analyser la relation entre variables comme la mention au Bac, les notes en mathématiques, le sexe, ou le type de Bac, et la réussite.  
- Réaliser des tests statistiques pour mesurer la dépendance entre variables.  
- Fournir une base solide pour une modélisation prédictive de la réussite des étudiants.

## 📁 Structure du projet
```
📁 analyse_reussite_L1_EcoGest
├─ 📄 rapport_statistique.pdf
├─ 📄 script_analyse.R
└─ 📝 README.md
```

## 🔧 Outils & Technologies
- R (statistiques, tests de dépendance)  
- RStudio  
- Packages R : dplyr, ggplot2, stats, etc.

## 📂 Données
Les données utilisées proviennent des pré-inscriptions en ligne des étudiants en L1 Économie-Gestion des deux dernières années dans les universités du Sud-Ouest. La population étudiée est composée de 2029 étudiants.

## 🔍 Méthodologie
**Analyse descriptive**  
Étude des caractéristiques démographiques, sociales et scolaires (genre, milieu social, séries de Bac, mentions, notes).

**Analyse bivariée**  
Tests statistiques (chi², test exact de Fisher, etc.) pour étudier les relations de dépendance entre variables explicatives et la réussite.

**Identification des variables déterminantes**  
Mise en évidence des facteurs ayant un effet significatif, notamment la mention obtenue au Bac et les notes en mathématiques.

## 📈 Résultats clés

| Variable                  | Relation avec réussite    |
|---------------------------|--------------------------|
| Mention au Baccalauréat    | Très significative       |
| Notes en Mathématiques     | Très significative       |
| Sexe                      | Relation modérée         |
| Retard au Baccalauréat    | Relation modérée         |
| Type de Baccalauréat      | Peu influent             |

## 📄 Rapport complet
Le rapport complet, incluant toutes les analyses descriptives, bivariées et conclusions, est disponible ici :  
👉 rapport_statistique.pdf

## 🧠 Ce que j’ai appris
- Manipulation et analyse statistique de données réelles d’étudiants.  
- Application de tests d’hypothèse pour détecter les relations entre variables.  
- Synthèse claire des résultats en vue d’une application pratique par les Rectorats.

## 📌 Pistes d'amélioration
- Extension de l’analyse à des modèles prédictifs (régression logistique, arbres décisionnels).  
- Intégration d’autres variables explicatives.  
- Automatisation de la pré-inscription basée sur la modélisation.

## 👤 Auteurs 
YOUCEF BELABBAS
ASSIFAR MEHDI IBRAHIM

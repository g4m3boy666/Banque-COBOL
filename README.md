# Banque-COBOL

[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/g4m3boy666/Banque-COBOL)

Petit projet réalisé en **COBOL** ayant pour objectif de recréer un **environnement bancaire simple**.

## À propos du projet

Ce projet représente mon premier véritable projet en COBOL.  
L’idée est de simuler le fonctionnement basique d’une banque à travers différentes opérations simples, afin de mieux comprendre :

- la structure d’un programme COBOL
- la gestion des données
- les conditions et traitements
- la logique métier d’un système bancaire simple

## Objectif

Le but de ce projet est de m’entraîner à développer une application concrète en COBOL, en m’appuyant sur un thème clair : **la gestion bancaire**.

À travers ce projet, je cherche à renforcer mes compétences sur :

- la syntaxe COBOL
- l’organisation du code
- la logique algorithmique
- la manipulation d’informations liées à des comptes ou opérations bancaires

## Fonctionnalités

Selon l’avancement du projet, on peut y retrouver par exemple :

- création ou gestion de comptes
- consultation du solde
- dépôt d’argent
- retrait d’argent
- affichage des informations bancaires

## Pourquoi ce projet ?

J’ai voulu réaliser un projet simple mais concret pour mieux apprendre COBOL en travaillant sur un cas pratique.  
Le domaine bancaire est intéressant car il permet de manipuler des données, des calculs et des règles de gestion de manière logique.

## Technologies utilisées

- **COBOL**

## Lancer dans GitHub Codespaces

Cliquez sur le bouton **Open in GitHub Codespaces** en haut du README pour ouvrir le projet directement dans GitHub.

Codespaces installe automatiquement **GnuCOBOL** et compile le programme à la création de l’environnement.

Pour lancer le programme dans le terminal Codespaces :

```bash
./main
```

Pour recompiler manuellement :

```bash
cobc -x -free main.cob
```

## Fonctionnalités

- Gestion de compte (basique)
- Consultation du solde
- Dépôt d’argent
- Retrait d’argent

## Prérequis

Avant de compiler le projet, vous devez installer :

- **GnuCOBOL**

### Installation de GnuCOBOL

#### Linux (Debian/Ubuntu)
```bash
sudo apt install gnucobol
```
#### Linux (Arch linux)
```bash
yay -S gnucobol
```
#### Linux (Fedora)
```bash
sudo dnf install gnucobol
```
#### Linux (openSUSE)
```bash
sudo zypper install gnucobol
```

## Auteur

Projet réalisé par **Neo (moi)**.

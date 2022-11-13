# Mallaka-language

Interpreter for the self-made language Mallaka. 
<br>
# Installation
Read the following instructions to install the Mallaka Language Interpreter. 
<br>
## Installation: Cloning the repository 

First install the github repository with the following command:

```shell
git clone: https://github.com/TobiasvandenHoogen/Mallaka-language.git
```
<br>

## Installation: Cabal  
For the Mallaka Language interpreter you also need Cabal. If you don't have Cabal use the following instructions depending on your operating system. 
<br>

### Installation Windows 

First install Chocolatey so that the Haskell package can be installed. Run these following commands on Powershell with administrative rights. 

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
```

Then with Chocolatey install haskell with cabal. 

```powershell
choco install ghc
```
<br>

### Installation Ubuntu 

To install Cabal use the following command. 
```shell
sudo apt-get install cabal-install
```
<br>

### Installation Mac 

First install Brew so that the Haskell package can be installed. 

```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Then install Cabal with the following command:
```shell
brew install ghc cabal-install
```
<br>

## Installation: Packages 

<br>

# Run 

To run the interpreter. Simply run the following command within the repository. 

```shell
cabal run 
```
<br>

# Test
Run the following command to run the unit, integration and system tests within the repository. 
```shell
cabal test
```
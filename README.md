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



<br>

# Run 

To run the interpreter. Simply run the following command within the repository. 

```shell
cabal run 
```
<br>

# Test
If you want to test the interpreter. You must install the following packages:
```shell
cabal install tasty --lib
cabal install tasty-hunit --lib
```
<br>

Run the following command to run the unit, integration and system tests within the repository. 
```shell
cabal test
```

# Doc 
Some parts of the code is commented with Haddock. To generate these comments in a HTML based view use the following commands. 

```shell
cabal haddock --haddock-all
cd dist-newstyle/t/doc
```

# Usage
The Mallaka Language offers the following functionalities.

<br>

## Variable deceleration  

```
a = 10;
b = 10.0;
c = True;
d = "Hello";
e = [1, 2, 3];
```
<br>

## Mathematical operations 

```
10 + 9.0;
50 - 170.8;
5 * 7;
10.0 / 2.0;
carrot 81; @square root
5 ^ 10;
```

## List operations 
```
[1, 2, 3] + [4, 5] @[1,2,3,4,5]
[1, 2, 3] - 2 @[1,2]
[1, 2, 3] * 2 @ [1,2,3,2]
carrot [1,2,3] @3
```

## If statements 

```
whatif(a == 1) ->10;<- 
orwhatif(a == 2) ->20;<-
orelse->30;<-;
```

## For loops

```
from 0 to 100 with 1 ->(a = a + 1;)<-;
```

## Until loops
```
until(a >= 100) ->(a = a + 1;)<-;
```

## Functions
```
process a {b} ->b + 5;<-;
run a {10};
```

## Include files
```
include "quicksort.ma";
```

## Print statement
```
a = [1,2,3]; see a;
```

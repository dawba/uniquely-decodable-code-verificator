# Uniquely Decodable Code Verificator
Haskell program that verifies if given code is uniquely decodable.

Given codeword set is obtained from a regular expression e.g. `a(bc+d)^2`.

For obtained codeword set, Sardinas-Patterson algorithm is applied to verify its unique decodability.

## Usage
```
# Clone repository
git clone https://github.com/dawba/uniquely-decodable-code-verificator.git

# Change directory
cd uniquely-decodable-code-verificator

# Compile with `ghc` without aux files
ghc main.hs -no-keep-o-files -no-keep-hi-files
```
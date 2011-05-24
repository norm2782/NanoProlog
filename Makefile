dist:
	cabal clean
	cabal configure
	cabal haddock
	cabal sdist

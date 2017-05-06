"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
installAddedClasses
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	addedClasses do: [:each | each installClassInSystem]
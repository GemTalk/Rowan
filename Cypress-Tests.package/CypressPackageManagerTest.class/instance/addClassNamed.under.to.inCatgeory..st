private
addClassNamed: className under: superClass to: aSymbolDictionary inCatgeory: aString

	| newClass |
	newClass := (superClass
				subclass: className
				instVarNames: #()
				classVars: #()
				classInstVars: #()
				poolDictionaries: #()
				inDictionary: aSymbolDictionary
				options: #()) category: aString.
	self classToCleanUp: newClass from: aSymbolDictionary.
	^newClass
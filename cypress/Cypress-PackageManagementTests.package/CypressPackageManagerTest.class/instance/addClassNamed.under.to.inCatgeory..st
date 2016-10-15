"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
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
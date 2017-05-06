"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
newOrExistingSymbolDictionaryNamed: dictName
	"If the current session's transient symbol list includes a dictionary with the given name, answer it.
	Otherwise, create one, add it to the end of both transient and persistent symbol lists, and answer it."

	| symbolName session symbolList association |
	symbolName := dictName asSymbol.
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	^symbolList
		detect: [:each | (each at: symbolName ifAbsent: [nil]) == each]
		ifNone: 
			[| newDict |
			newDict := (SymbolDictionary new)
						name: symbolName;
						objectSecurityPolicy: symbolList objectSecurityPolicy;
						yourself.
			symbolList addLast: newDict.
			session userProfile symbolList addLast: newDict.
			newDict]
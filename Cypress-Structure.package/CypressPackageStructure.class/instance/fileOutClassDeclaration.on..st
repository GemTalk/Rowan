filing out - private
fileOutClassDeclaration: classStructure on: aStream

	aStream
		nextPutAll: 'doit'; lf;
		nextPutAll: '(', classStructure superclassName; lf;
		nextPutAll: '	subclass: ', classStructure className asString printString; lf;
		nextPutAll: '	instVarNames: #( ', classStructure instanceVariablesString, ' )'; lf;
		nextPutAll: '	classVars: #( ', classStructure classVariablesString, ' )'; lf;
		nextPutAll: '	classInstVars: #( ', classStructure classInstanceVariablesString, ' )'; lf;
		nextPutAll: '	poolDictionaries: #()'; lf;
		nextPutAll: '	inDictionary: UserGlobals'; lf;
		nextPutAll: '	options: #())'; lf;
		nextPutAll: '		category: ', classStructure category printString, ';'; lf;
		nextPutAll: '		comment: ', classStructure comment printString, ';'; lf;
		nextPutAll: '		immediateInvariant.'; lf;
		nextPutAll: '%'; lf;
		lf.

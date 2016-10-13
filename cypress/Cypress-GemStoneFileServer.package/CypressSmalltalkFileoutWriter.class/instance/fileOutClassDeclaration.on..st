"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutClassDeclaration: classStructure on: aStream
	"This is the structure for VW/Pharo  class definitions. It may or may not be portable
	to all dialects that use chunk-format fileins. It does not attempt to handle VW
	namespaces at this time. Probably should use an XML fileout for that."

	aStream
		nextPutAll: classStructure superclassName;
		nextPutAll: ' subclass: #' nextPutAll: classStructure className;
		lf;
		nextPutAll: '	instanceVariableNames:  ''';
		nextPutAll: classStructure instanceVariablesString;
		nextPut: $';
		lf;
		nextPutAll: '	classVariableNames: ''';
		nextPutAll: classStructure classVariablesString;
		nextPut: $';
		lf;
		nextPutAll: '	poolDictionaries: ''';
		nextPutAll: classStructure poolDictionariesString;
		nextPut: $';
		lf;
		nextPutAll: '	category: ''' nextPutAll: classStructure category;
		nextPut: $'
"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Comparison
"
classDefinitionString

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPut: $(;
		nextPutAll: superclassName;
		space;
		nextPutAll: self classCreationSelector;
		space;
		nextPutAll: self name printString.
	self subclassType = 'byteSubclass'
		ifFalse: 
			[stream
				lf;
				tab;
				nextPutAll: 'instVarNames: #(' , self instanceVariablesString , ')'].
	stream
		lf;
		tab;
		nextPutAll: 'classVars: #(' , self classVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'classInstVars: #(' , self classInstanceVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'poolDictionaries: #(' , self poolDictionariesString , ')';
		lf;
		tab;
		nextPutAll: 'inDictionary: ''<not-defined>''';
		nextPut: $);
		lf;
		tab;
		tab;
		nextPutAll: 'category: ' , self category printString , ';';
		lf;
		tab;
		tab;
		nextPutAll: 'comment: ' , self comment printString.
	^stream contents

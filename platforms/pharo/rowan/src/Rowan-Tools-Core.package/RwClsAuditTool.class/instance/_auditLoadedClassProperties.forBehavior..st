audit
_auditLoadedClassProperties: aLoadedClass forBehavior: aBehavior
"Check #( 'instvars', 'superclass', 'classinstvars',  'gs_SymbolDictionary', 'comment', 'classvars', 'pools', 'category')"

	| res  aDict |
	res :=  self _result.
	((aLoadedClass propertyAt: 'superclass') isEquivalent: aBehavior superclass name ) 
		ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Superclass is different from loaded class'].
	((aLoadedClass propertyAt: 'instvars') = (aBehavior instVarNames collect: [:e | e asString]) ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'instVarNames changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'classvars') = ((aBehavior.classVars ifNil: [SymbolDictionary new]) 
			keys collect: [:e | e asString]) asSortedCollection asArray) ifFalse: [
				self errorLog: res  add: aLoadedClass name -> 'ClassVars changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'pools') = ((aBehavior.poolDictionaries ifNil: [Array new]) collect: [:e | e asString]) ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'PoolDictionaries changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'comment' ifAbsent: ['']) isEquivalent: aBehavior rwComment ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Comment has changed in compiled class v loaded class'].
	((aLoadedClass propertyAt: 'category') = aBehavior category ) 
			ifFalse: [self errorLog: res  add: aLoadedClass name -> 'Class category has changed in compiled class v loaded class'].
	(aDict := System myUserProfile resolveSymbol: (aLoadedClass propertyAt: 'gs_SymbolDictionary') asSymbol ) 
			ifNil: [self errorLog: res  add: aLoadedClass name -> ('Unable to find SymbolDictionary ' ,(aLoadedClass propertyAt: 'gs_SymbolDictionary'))] 
			ifNotNil: [:smbd | smbd value at: aLoadedClass name asSymbol 
					ifAbsent: [self errorLog: res  add: aLoadedClass name -> 'Compiled class not found in symbol dictionary of loaded class']] .

	^res
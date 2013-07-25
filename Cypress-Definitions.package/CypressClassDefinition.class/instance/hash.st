comparing
hash

	| hash |
	hash := name hash.
	hash := superclassName hash bitOr: hash.
	hash := (category ifNil: ['']) hash bitOr: hash.
	instVarNames , classInstVarNames, classVarNames, poolDictionaryNames
		do: [:vName | hash := vName hash bitOr: hash].
	^hash
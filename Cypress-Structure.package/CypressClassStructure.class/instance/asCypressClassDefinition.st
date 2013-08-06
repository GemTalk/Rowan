"
notice: This work is protected by copyright. All rights reserved.
category: converting
"
asCypressClassDefinition

	self isClassExtension ifTrue: [^CypressError signal: 'Extensions cannot have class definitions'].
	^CypressClassDefinition
		name: self className
		superclassName: self superclassName
		category: self category
		instVarNames: self instanceVariableNames
		classInstVarNames: self classInstanceVariableNames
		classVarNames: self classVariableNames
		poolDictionaryNames: self poolDictionaryNames
		comment: self comment
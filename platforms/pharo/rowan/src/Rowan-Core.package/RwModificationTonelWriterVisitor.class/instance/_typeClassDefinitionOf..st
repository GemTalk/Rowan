class writing
_typeClassDefinitionOf: aClassDefinition
	| definition |
	
	definition := self class orderedDictionaryClass new 
		at: #name put: aClassDefinition name; 
		at: #superclass put: aClassDefinition superclassName;
		yourself.

	aClassDefinition classType = 'normal' ifFalse: [ 
		definition at: #type put: aClassDefinition classType ].
	
	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instVars put: vars asArray ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classVars put: vars asArray ].
		
	((aClassDefinition poolDictionaryNames) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].
		
	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classInstVars put: vars asArray ].

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	definition 		
		at: #category put: aClassDefinition category.
	
	^ self _toSTON: definition
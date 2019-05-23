class writing
_typeClassDefinitionOf: aClassDefinition
	"order is alphabetical"

	| definition |
	definition := self class orderedDictionaryClass new.

	definition 		
		at: #category put: aClassDefinition category.

	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classinstvars put: vars asArray ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classvars put: vars asArray ].

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instvars put: vars asArray ].

	definition at: #name put: aClassDefinition name.

	((aClassDefinition poolDictionaryNames) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].

	definition at: #super put: aClassDefinition superclassName.

	definition at: #type put: aClassDefinition classType.

	^ self _toJSON: definition
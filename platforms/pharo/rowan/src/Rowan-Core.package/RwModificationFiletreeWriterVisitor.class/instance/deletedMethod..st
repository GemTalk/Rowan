actions
deletedMethod: aMethodModification

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodModification before.

	"delete method source file"
	 (aMethodModification isMeta
		ifTrue: [
			methodFileName := classFileNameMap at: methodDefinition selector
				ifAbsent: [ classBeforeFileNameMap at: methodDefinition selector ].
			methodDir := self _methodClassDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceFileNameMap at: methodDefinition selector
				ifAbsent: [ instanceBeforeFileNameMap at: methodDefinition selector ].
			methodDir := self _methodInstanceDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st') ensureDelete
actions
deletedMethodExtension: aMethodExtensionModification

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodExtensionModification before.

	"create instance/class directory as needed ... write method source file"
	 (aMethodExtensionModification isMeta
		ifTrue: [
			methodFileName := classBeforeFileNameMap at: methodDefinition selector.
			methodDir := self _methodClassExtensionDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceBeforeFileNameMap at: methodDefinition selector.
			methodDir := self _methodInstanceExtensionDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st') ensureDelete
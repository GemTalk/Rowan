actions
addedMethodExtension: aMethodExtensionModification

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodExtensionModification after.

	"create instance/class directory as needed ... write method source file"
	 (aMethodExtensionModification isMeta
		ifTrue: [
			methodFileName := classFileNameMap at: methodDefinition selector.
			methodDir := self _methodClassExtensionDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceFileNameMap at: methodDefinition selector.
			methodDir := self _methodInstanceExtensionDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st')
		writeStreamDo: [:aStream |
		aStream 
			<< methodDefinition protocol
			<< self _newLine
			<< (methodDefinition source withLineEndings: self _newLine) ]
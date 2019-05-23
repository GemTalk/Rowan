actions
addedMethod: aMethodModification

	| methodDefinition methodDir methodFileName |

	methodDefinition := aMethodModification after.

	"create instance/class directory as needed ... write method source file"
	 (aMethodModification isMeta
		ifTrue: [
			methodFileName := classFileNameMap at: methodDefinition selector.
			methodDir := self _methodClassDir ensureCreateDirectory ]
		ifFalse: [ 
			methodFileName := instanceFileNameMap at: methodDefinition selector.
			methodDir := self _methodInstanceDir ensureCreateDirectory ]) .


	(methodDir / methodFileName, 'st')
		writeStreamDo: [:aStream |
		aStream 
			<< methodDefinition protocol
			<< self _newLine
			<< (methodDefinition source withLineEndings: self _newLine) ]
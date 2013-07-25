utilities
readStreamFor: filePath do: aOneArgBlock

	| file |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	[aOneArgBlock value: file] ensure: [file close]
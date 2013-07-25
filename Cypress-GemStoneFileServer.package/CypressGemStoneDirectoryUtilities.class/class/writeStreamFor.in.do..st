utilities
writeStreamFor: filePath in: aDirectory do: aOneArgBlock

	| file |
	GsFile serverErrorString.
	file := GsFile openWriteOnServer: (self directoryFromPath: filePath relativeTo: aDirectory).
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	[aOneArgBlock value: file] ensure: [file close]
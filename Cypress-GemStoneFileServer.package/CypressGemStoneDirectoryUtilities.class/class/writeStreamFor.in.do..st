"
notice: This work is protected by copyright. All rights reserved.
category: utilities
"
writeStreamFor: filePath in: aDirectory do: aOneArgBlock

	| file stream |
	GsFile serverErrorString.
	file := GsFile openWriteOnServer: (self directoryFromPath: filePath relativeTo: aDirectory).
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	stream := WriteStream on: String new.
	[aOneArgBlock value: stream] ensure: [file nextPutAll: stream contents encodeAsUTF8; close]
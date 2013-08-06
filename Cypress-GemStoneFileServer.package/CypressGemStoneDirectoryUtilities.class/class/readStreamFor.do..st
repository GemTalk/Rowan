"
notice: This work is protected by copyright. All rights reserved.
category: utilities
"
readStreamFor: filePath do: aOneArgBlock

	| file stream |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	[stream := ReadStream on: (String withAll: file contents asByteArray decodeFromUTF8).
	aOneArgBlock value: stream] ensure: [file close]
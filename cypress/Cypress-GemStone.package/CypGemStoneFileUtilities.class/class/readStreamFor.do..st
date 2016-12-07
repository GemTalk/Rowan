"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utilities
"
readStreamFor: filePath do: aOneArgBlock

	| file stream |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString
		ifNotNil: [:errorMessage | self error: errorMessage].
	
	[stream := ReadStreamPortable
				on: (String withAll: file contents asByteArray decodeFromUTF8).
	aOneArgBlock value: stream]
			ensure: [file close]
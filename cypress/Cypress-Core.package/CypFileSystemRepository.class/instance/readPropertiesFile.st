"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readPropertiesFile

	| filename |
	filename := self propertiesFileName.
	(self doesRepositoryFileExist: filename)
		ifTrue: [self readPropertiesFile: filename]
		ifFalse: [self reactToAbsentPropertiesFile]
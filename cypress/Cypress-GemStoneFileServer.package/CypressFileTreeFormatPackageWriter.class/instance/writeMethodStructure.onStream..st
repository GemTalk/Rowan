"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: methodStructure category; lf;
		nextPutAll: methodStructure source withUnixLineEndings

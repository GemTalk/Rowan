"
notice: This work is protected by copyright. All rights reserved.
category: writing
"
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: methodStructure category; lf;
		nextPutAll: methodStructure source withUnixLineEndings

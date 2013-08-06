"
notice: This work is protected by copyright. All rights reserved.
category: writing
"
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: '"'; lf;
		nextPutAll: 'notice: ', self methodNoticeLine; lf;
		nextPutAll: 'category: ', methodStructure category; lf;
		nextPutAll: '"'; lf;
		nextPutAll: methodStructure source withUnixLineEndings
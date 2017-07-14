"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
writeClassComment: classStructure on: fileStream

	fileStream
		nextPutAll: (CypressObject normalizeLineEndingsOf: classStructure comment)

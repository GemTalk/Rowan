tests
testAsRwUrlForFileUrls

	self sampleFileUrlStrings
		do: [ :ar | 
			| url urlString fileString pathString dirString x |
			urlString := ar at: 1.
			fileString := ar at: 2.
			RwUrl fromString: urlString.
			url := urlString asRwUrl.
			pathString := url pathString.
			dirString := url pathDirString.
			self assert: (pathString = fileString).
			self assert: url printString = urlString.
			pathString last = $/
				ifTrue: [  self assert: dirString = pathString ]
				ifFalse: [ 
					"use asFileReference to expand env vars in path"
					self assert: (dirString asFileReference pathString) = (x :=  pathString asFileReference parent pathString) ] ]
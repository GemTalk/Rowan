"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: printing
"
printOn: aStream

	aStream nextPutAll: self schemeName , '://' , self host.
	aStream
		nextPutAll: self projectPath;
		nextPut: $:.
	String self projectBranchOrTag do: 
			[:char |
			char = $/ ifTrue: [aStream nextPut: $\].
			aStream nextPut: char].
	aStream
		nextPut: $/;
		nextPutAll: self repositoryPath

"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageWriter',
	#superclass : 'CypressAbstractPackageWriter',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'accessing - private' }
CypressPackageWriter >> methodNoticeLine [

	^self packageStructure properties
		at: 'copyrightLine'
		ifAbsent: [self repository copyrightProperty]

]

{ #category : 'writing' }
CypressPackageWriter >> writeMethodStructure: methodStructure onStream: fileStream [

	fileStream
		nextPutAll: '"';
		lf;
		nextPutAll: 'notice: ' , self methodNoticeLine;
		lf;
		nextPutAll: 'category: ' , methodStructure category;
		lf;
		nextPutAll: '"';
		lf;
		nextPutAll: (CypressObject normalizeLineEndingsOf: methodStructure source)

]

{ #category : 'writing' }
CypressPackageWriter >> writePropertiesFile [

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | self repository properties _writeCypressJsonOn: fileStream]

]

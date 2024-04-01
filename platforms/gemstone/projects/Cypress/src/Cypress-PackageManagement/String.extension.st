Extension { #name : 'String' }

{ #category : '*Cypress-PackageManagement' }
String >> _writeCypressJsonOn: aStream indent: startIndent [
	"Private method which may be removed in a future GemStone version."

	aStream nextPutAll: '"'.
	aStream nextPutAll: (CypressObject normalizeLineEndingsOf: self).
	aStream nextPutAll: '"'
]

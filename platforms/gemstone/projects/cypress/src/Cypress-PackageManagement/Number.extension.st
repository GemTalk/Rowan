Extension { #name : 'Number' }

{ #category : '*Cypress-PackageManagement' }
Number >> _writeCypressJsonOn: aStream indent: startIndent [
	"Private method which may be removed in a future GemStone version."

	aStream nextPutAll: self printString

]

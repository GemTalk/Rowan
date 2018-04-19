Extension { #name : 'Object' }

{ #category : '*Cypress-PackageManagement' }
Object >> _writeCypressJsonOn: fileStream [
	"Private method which may be removed in a future GemStone version."

	self _writeCypressJsonOn: fileStream indent: 0

]

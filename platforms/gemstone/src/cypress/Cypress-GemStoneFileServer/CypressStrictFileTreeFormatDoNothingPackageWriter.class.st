"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressStrictFileTreeFormatDoNothingPackageWriter',
	#superclass : 'CypressAbstractPackageWriter',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'writing' }
CypressStrictFileTreeFormatDoNothingPackageWriter >> removeOldReplacingWithNew [
	"Change nothing, since a Cypress writer has insufficient information
	 for preserving the FileTree details. Strictly read-only."

]

{ #category : 'writing' }
CypressStrictFileTreeFormatDoNothingPackageWriter >> writePropertiesFile [
	"Change nothing, since a Cypress writer has insufficient information
	 for preserving the FileTree details. Strictly read-only."

]

Extension { #name : 'CypressGemStoneDirectoryUtilities' }

{ #category : '*cypresstonel-core' }
CypressGemStoneDirectoryUtilities class >> default [
  ^ self workingDirectory

]

{ #category : '*cypresstonel-core' }
CypressGemStoneDirectoryUtilities class >> entryNamesFrom: aDirectory [
  "Answer just the name of the contents of aDirectory."

  ^ (((self directoryEntriesFrom: aDirectory)
    collect: [ :each | self localNameFrom: each ])
    reject: [ :each | each = '.' or: [ each = '..' ] ])
    sortWithBlock: [ :a :b | a <= b ]

]

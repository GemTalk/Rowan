Extension { #name : 'CypressTonelRepository' }

{ #category : '*rowan-cypress-kernel' }
CypressTonelRepository >> packageNames [

	"only directories with a package.st file in them"

	| utils |
	utils := self fileUtils.
	^ (((utils directoryEntriesFrom: self directoryPath , '*')
		reject: [ :each | 
			| aGsFileStat |
			aGsFileStat := GsFile _stat: each isLstat: false.
			aGsFileStat _isSmallInteger
				ifTrue: [ false ]
				ifFalse: [ aGsFileStat isDirectory not ] ])
		collect: [ :each | utils localNameFrom: each ])
		reject: [ :each | each = '.' or: [ each = '..' ] ]
]

Extension { #name : 'GsFile' }

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> currentWorkingDirectoryPath [

	^self _expandEnvVariable: 'PWD' isClient:false

]

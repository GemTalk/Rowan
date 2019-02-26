Extension { #name : 'GsFile' }

{ #category : '*filesystem-gemstone-kernel' }
GsFile >> fullName [
	^self pathName

]

{ #category : '*FileSystem-Gemstone-Kernel' }
GsFile class >> currentWorkingDirectoryPath [

	^self _expandEnvVariable: 'PWD' isClient:false

]

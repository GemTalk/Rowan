Extension { #name : 'GsFile' }

{ #category : '*filesystem-gemstone-kernel-35x' }
GsFile class >> _contentsOfServerDirectory: aPathName expandPath: aBoolean [

	^ self _contentsOfServerDirectory: aPathName expandPath: aBoolean utf8Results: false
]

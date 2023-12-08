Extension { #name : 'GsFile' }

{ #category : '*rowan-tonel-gemstone-kernel' }
GsFile >> << items [

 	items putOn: self.
	
	^ self

]

{ #category : '*rowan-tonel-gemstone-kernel' }
GsFile >> wrappedStreamName [
	^ self pathName
]

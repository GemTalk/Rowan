Extension { #name : 'Behavior' }

{ #category : '*ast-kernel-core' }
Behavior >> parseTreeFor: aSymbol [	
	^ RBParser parseMethod: (self sourceCodeAt: aSymbol) onError: [ :msg :pos | ^ nil ]
]

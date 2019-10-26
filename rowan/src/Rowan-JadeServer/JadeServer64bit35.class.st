Class {
	#name : 'JadeServer64bit35',
	#superclass : 'JadeServer64bit32',
	#category : 'Rowan-JadeServer'
}

{ #category : 'other' }
JadeServer64bit35 >> stepThrough: aGsProcess inFrame: anInteger [
  aGsProcess stepThroughFromLevel: anInteger
]

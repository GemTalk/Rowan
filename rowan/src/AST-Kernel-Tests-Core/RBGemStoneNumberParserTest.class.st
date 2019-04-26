Class {
	#name : 'RBGemStoneNumberParserTest',
	#superclass : 'TestCase',
	#category : 'AST-Kernel-Tests-Core'
}

{ #category : 'tests' }
RBGemStoneNumberParserTest >> testNumberParser [

	"Bugs in Number class>>fromStream: that needed to be fixed for use in RBParser"

	self assert: (Number rwFromStream: '1.2' readStream) = 1.2.
	self assert: (Number rwFromStream: '1.' readStream) = 1.0.
	self assert: (Number rwFromStream: '1' readStream) = 1.
	self assert: (Number rwFromStream: '16r10000"' readStream) =  65536.
]

{ #category : 'tests' }
RBGemStoneNumberParserTest >> testRadixNumberParser [

	"Bugs in Number class>>fromStream: that needed to be fixed for use in RBParser"

	| x |
	self assert: (x := Number rwFromStream: '11rA' readStream) = 10.
]

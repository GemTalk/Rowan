Class {
	#name : 'ZnDefaultCharacterEncoder',
	#superclass : 'DynamicVariable',
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
ZnDefaultCharacterEncoder >> default [
	^ ZnCharacterEncoder utf8
]

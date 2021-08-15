Class {
	#name : 'TonelCypressWriterTest',
	#superclass : 'TonelAbstractWriterTest',
	#category : 'CypressTonel-Tests'
}

{ #category : 'private' }
TonelCypressWriterTest >> fileUtils [
  ^ CypressFileUtilities current

]

{ #category : 'mocks' }
TonelCypressWriterTest >> mockSnapshot [

	^ self mockCypressSnapshot

]

{ #category : 'private' }
TonelCypressWriterTest >> writerClass [

  ^ TonelCypressWriter

]

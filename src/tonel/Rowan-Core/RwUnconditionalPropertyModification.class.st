"
No class-specific documentation for CypPropertyModification, hierarchy is: 
Object
  CypDiff( changes)
    CypLeafModification( key before after)
      CypPropertyModification

"
Class {
	#name : 'RwUnconditionalPropertyModification',
	#superclass : 'RwPropertyModification',
	#category : 'Rowan-Core'
}

{ #category : 'testing' }
RwUnconditionalPropertyModification >> isEmpty [

	^ false
]

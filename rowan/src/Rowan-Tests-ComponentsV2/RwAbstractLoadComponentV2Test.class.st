Class {
	#name : 'RwAbstractLoadComponentV2Test',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-ComponentsV2'
}

{ #category : 'private' }
RwAbstractLoadComponentV2Test >> _validateComponent: component condition: condition componentNames: componentNames packageNames: packageNames projectNames: projectNames [
	| x |
	self assert: (x := component condition) = condition.
	self assert: (x := component componentNames asArray sort) = componentNames sort.
	self assert: (x := component packageNames asArray sort) = packageNames sort.
	self assert: (x := component projectNames asArray sort) = projectNames sort
]

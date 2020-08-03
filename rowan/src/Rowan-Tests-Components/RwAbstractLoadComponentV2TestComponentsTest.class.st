Class {
	#name : 'RwAbstractLoadComponentV2TestComponentsTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-Components'
}

{ #category : 'private' }
RwAbstractLoadComponentV2TestComponentsTest >> _validateComponent: component condition: conditionArray group: groupName componentNames: componentNames packageNames: packageNames projectNames: projectNames [
	| x |
	self
		assert:
			(x := component
				conditionalComponentsAtConditions: conditionArray
				andGroup: groupName) asArray = componentNames.
	self
		assert:
			(x := component
				conditionalPackagesAtConditions: conditionArray
				andGroup: groupName) asArray = packageNames.
	self
		assert:
			(x := component
				conditionalProjectsAtConditions: conditionArray
				andGroup: groupName) asArray = projectNames
]

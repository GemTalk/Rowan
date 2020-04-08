Class {
	#name : 'RwAbstractLoadComponentV2Test',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-DiskConfigurationsV2'
}

{ #category : 'private' }
RwAbstractLoadComponentV2Test >> _validateComponent: component condition: conditionArray group: groupName componentNames: componentNames packageNames: packageNames projectNames: projectNames [
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

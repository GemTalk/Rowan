Class {
	#name : 'CypressEnvironmentDependencySorter',
	#superclass : 'CypressDependencySorter',
	#category : 'Cypress-Environmental-Tools'
}

{ #category : 'building' }
CypressEnvironmentDependencySorter >> add: aPatchOperation [
  | requirements |
  requirements := self unresolvedRequirementsFor: aPatchOperation.
  requirements removeIfPresent: 'nil'.
  requirements removeIfPresent: nil.
  requirements isEmpty
    ifTrue: [ self addToOrder: aPatchOperation ]
    ifFalse: [ self addRequirements: requirements for: aPatchOperation ].
  ^ aPatchOperation

]

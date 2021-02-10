Class {
	#name : 'RwPackageMove',
	#superclass : 'RwMove',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwPackageMove class >> packageBefore: beforePackageDefinition packageAfter: afterPackageDefinition projectBefore: beforeProjectDefinition projectAfter: afterProjectDefinition [
	^ self new
		packageBefore: beforePackageDefinition;
		packageAfter: afterPackageDefinition;
		projectBefore: beforeProjectDefinition;
		projectAfter: afterProjectDefinition;
		yourself
]

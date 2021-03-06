Extension { #name : 'Rowan' }

{ #category : '*rowan-coreV2' }
Rowan class >> newEmbeddedProjectNamed: projectName [
	^ self platform newEmbeddedProjectNamed: projectName
]

{ #category : '*rowan-coreV2' }
Rowan class >> newProjectNamed: projectName [
	^ self platform newProjectNamed: projectName
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl [
	^ self platform projectFromUrl: loadSpecUrl
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl diskUrl: urlString [
	^ self platform projectFromUrl: loadSpecUrl diskUrl: urlString
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl gitUrl: urlString [
	^ self platform projectFromUrl: loadSpecUrl gitUrl: urlString
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome [
	^ self platform projectFromUrl: loadSpecUrl projectsHome: projectsHome
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames [
	^ self platform
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		componentNames: componentNames
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames customConditionalAttributes: customConditionalAttributes [
	^ self platform
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		componentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	^ self platform
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		componentNames: componentNames
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl projectsHome: projectsHome customConditionalAttributes: customConditionalAttributes [
	^ self platform
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		customConditionalAttributes: customConditionalAttributes
]

{ #category : '*rowan-coreV2' }
Rowan class >> projectFromUrl: loadSpecUrl readonlyDiskUrl: urlString [
	^ self platform projectFromUrl: loadSpecUrl readonlyDiskUrl: urlString
]

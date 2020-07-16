Extension { #name : 'RwPlatform' }

{ #category : '*rowan-corev2' }
RwPlatform >> newEmbeddedProjectNamed: projectName [
	^ RwDefinedProject newEmbeddedProjectNamed: projectName
]

{ #category : '*rowan-corev2' }
RwPlatform >> newProjectNamed: projectName [
	^ RwDefinedProject newNamed: projectName
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl [
	^ RwResolvedProject projectFromUrl: loadSpecUrl
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl diskUrl: urlString [
	^ RwResolvedProject projectFromUrl: loadSpecUrl diskUrl: urlString
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl projectsHome: projectsHome [
	^ RwResolvedProject projectFromUrl: loadSpecUrl projectsHome: projectsHome
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames [ 
	^ RwResolvedProject
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		componentNames: componentNames
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames customConditionalAttributes: customConditionalAttributes [
	^ RwResolvedProject
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		componentNames: componentNames
		customConditionalAttributes: customConditionalAttributes
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl projectsHome: projectsHome componentNames: componentNames platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProject
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		componentNames: componentNames
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl projectsHome: projectsHome customConditionalAttributes: customConditionalAttributes [
	^ RwResolvedProject
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		customConditionalAttributes: customConditionalAttributes
]

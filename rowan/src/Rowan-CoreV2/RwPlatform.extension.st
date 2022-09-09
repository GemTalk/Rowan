Extension { #name : 'RwPlatform' }

{ #category : '*rowan-corev2' }
RwPlatform >> newProjectNamed: projectName [
	"Create a new project that uses Rowan v3 project spec and component classes"

	^ RwDefinedProject newNamed: projectName
]

{ #category : '*rowan-corev2' }
RwPlatform >> newV2ProjectNamed: projectName [
	"Create a new project that uses project spec and component classes that are compatible with Rowan v2"

	^ RwDefinedProject newV2Named: projectName
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
RwPlatform >> projectFromUrl: loadSpecUrl gitUrl: urlString [
	^ RwResolvedProject projectFromUrl: loadSpecUrl gitUrl: urlString
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
RwPlatform >> projectFromUrl: loadSpecUrl projectsHome: projectsHome customConditionalAttributes: customConditionalAttributes [
	^ RwResolvedProject
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		customConditionalAttributes: customConditionalAttributes
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl projectsHome: projectsHome customConditionalAttributes: customConditionalAttributes platformConditionalAttributes: platformConditionalAttributes [
	^ RwResolvedProject
		projectFromUrl: loadSpecUrl
		projectsHome: projectsHome
		customConditionalAttributes: customConditionalAttributes
		platformConditionalAttributes: platformConditionalAttributes
]

{ #category : '*rowan-corev2' }
RwPlatform >> projectFromUrl: loadSpecUrl readOnlyDiskUrl: urlString [
	^ RwResolvedProject projectFromUrl: loadSpecUrl readOnlyDiskUrl: urlString
]

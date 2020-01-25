Extension { #name : 'RwAbstractProjectComponentVisitorV2' }

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractProjectComponentVisitorV2 >> _components: componentDirPath forProject: aProjectName [
	| componentDirectory |
	self componentNames isEmpty
		ifTrue: [ ^ #() ].
	componentDirectory := componentDirPath asFileReference.
	^ self componentNames
		collect: [ :componentName | 
			(RwBasicProjectLoadComponentV2
				fromComponentsDirectory: componentDirectory
				named: componentName)
				projectName: aProjectName;
				yourself ]
]

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractProjectComponentVisitorV2 >> _projects: projectDirPath forProject: ignored [

	| urlBase |
	self projectNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,projectDirPath asFileReference pathString, '/'.
	^ self projectNames
		collect: [ :prjName | 
			| url |
			url := urlBase , prjName , '.ston'.
			RwSpecification fromUrl: url ]
]

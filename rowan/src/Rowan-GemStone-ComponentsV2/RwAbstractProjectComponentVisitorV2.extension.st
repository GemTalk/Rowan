Extension { #name : 'RwAbstractProjectComponentVisitorV2' }

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractProjectComponentVisitorV2 >> _components: componentDirPath forProject: aProjectName [
	| componentDirectory selected |
	self componentNames isEmpty
		ifTrue: [ ^ #() ].
	componentDirectory := componentDirPath asFileReference.
	selected := (self componentNames
		select: [ :componentName | (visitedComponentNames includes: componentName) not ])
		collect: [ :componentName | 
			self readComponents
				at: componentName
				ifAbsentPut: [ 
 					(RwAbstractRowanProjectLoadComponentV2
						fromComponentsDirectory: componentDirectory
						named: componentName)
						projectName: aProjectName;
						yourself ] ].
	^ selected
]

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractProjectComponentVisitorV2 >> _projects: projectDirPath forProject: ignored [
	| urlBase |
	self projectNames isEmpty
		ifTrue: [ ^ #() ].
	urlBase := 'file:' , projectDirPath asFileReference pathString , '/'.
	^ self projectNames
		collect: [ :prjName | 
			self readProjects
				at: prjName
				ifAbsentPut: [ 
					| url |
					url := urlBase , prjName , '.ston'.
					RwSpecification fromUrl: url ] ]
]

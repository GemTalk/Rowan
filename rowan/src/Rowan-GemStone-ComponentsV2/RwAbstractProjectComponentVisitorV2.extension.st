Extension { #name : 'RwAbstractProjectComponentVisitorV2' }

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractProjectComponentVisitorV2 >> _components: componentDirPath forProject: aProjectName [

	| urlBase |
	self componentNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,componentDirPath asFileReference pathString, '/'.
	^ self componentNames
		collect: [ :componentName | 
			| url |
			url := urlBase , componentName , '.ston'.
			(RwAbstractProjectLoadComponentV2 fromUrl: url)
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

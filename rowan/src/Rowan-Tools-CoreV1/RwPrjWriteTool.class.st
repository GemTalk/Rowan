Class {
	#name : 'RwPrjWriteTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV1'
}

{ #category : 'private' }
RwPrjWriteTool >> _loadedPackagesForProject: projectDefinition [

	"Answer a PackageSet containing packages for the given project definition."

	^ self
		_loadedPackagesNamed: projectDefinition packageNames
		forProject: projectDefinition

]

{ #category : 'private' }
RwPrjWriteTool >> _loadedPackagesNamed: someNames forProject: projectDefinition [

	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	"very likely we should be dealing with a project name at this point ... unless there is a use 
		case for loading a collection of packages without directly using the project interface 
		--- loaded packages do know what project they belong to ... so there might be something 
		done behind the scenes"

	| loadedPackages |
	loadedPackages := someNames
		collect: [ :aName | 
			projectDefinition packageNamed: aName ifAbsent: [ self error: 'package not a member of projectDefinition' ].
			Rowan image loadedPackageNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet
		withAll:
			(loadedPackages
				select: [ :loadedPackage | 
					loadedPackage notNil
						and: [ loadedPackage loadedProject name = projectDefinition name ] ])
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writeComponentProjectDefinition: projectDefinition [

	Rowan projectTools create createProjectRepository: projectDefinition projectRef.
	projectDefinition 
		exportProjects;
		exportComponents;
		exportPackages
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writeProjectDefinition: projectDefinition [

	projectDefinition export
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writeProjectNamed: projectName [

	| loadedProject projectDefinition |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	projectDefinition := loadedProject asDefinition.
	self writeProjectDefinition: projectDefinition.
	"loaded project and loaded packages written to disk - mark them not dirty"
	loadedProject markNotDirty.
	loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ].
]

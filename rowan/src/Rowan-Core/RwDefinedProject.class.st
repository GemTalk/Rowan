Class {
	#name : 'RwDefinedProject',
	#superclass : 'RwAbstractUnloadedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwDefinedProject class >> fromResolvedProject: aResolvedProject [
	^ (self newNamed: aResolvedProject name)
		_resolvedProject: aResolvedProject _resolvedProject;
		yourself
]

{ #category : 'private' }
RwDefinedProject >> _loadSpecification [
	^ self _resolvedProject _loadSpecification
]

{ #category : 'private' }
RwDefinedProject >> _resolvedProject [
	^ resolvedProject
		ifNil: [ 
			resolvedProject := RwResolvedProjectV2 new
				projectName: self name;
				yourself ]
]

{ #category : 'accessing' }
RwDefinedProject >> addComponentNamed: componentName toComponentNamed: toComponentName [
	^ self _resolvedProject
		addComponentNamed: componentName
		toComponentNamed: toComponentName
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: componentName [
	^ self _resolvedProject addNewComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: aComponentName comment: aString [
	^ self _resolvedProject addNewComponentNamed: aComponentName comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: aComponentName condition: condition comment: aString [
	^ self _resolvedProject
		addNewComponentNamed: aComponentName
		condition: condition
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addNewComponentNamed: aComponentName toComponentNamed: toComponentName condition: conditionPathArray comment: aString [
	^ self _resolvedProject
		addNewComponentNamed: aComponentName
		toComponentNamed: toComponentName
		condition: conditionPathArray
		comment: aString
]

{ #category : 'accessing' }
RwDefinedProject >> addPackageNamed: packageName toComponentNamed: componentName [
	^ self _resolvedProject
		addPackageNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addPackagesNamed: packageName toComponentNamed: componentName [
	^ self _resolvedProject
		addPackagesNamed: packageName
		toComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> addTopLevelComponentNamed: componentName [
	^ self _resolvedProject addTopLevelComponentNamed: componentName
]

{ #category : 'accessing' }
RwDefinedProject >> customConditionalAttributes: anArray [
	self _loadSpecification customConditionalAttributes: anArray
]

{ #category : 'testing' }
RwDefinedProject >> isStrict [
	^ self _resolvedProject isStrict
]

{ #category : 'accessing' }
RwDefinedProject >> packageNamed: aString [
	^ self _resolvedProject packageNamed: aString
]

{ #category : 'accessing' }
RwDefinedProject >> repoType: aSymbol [
	"#disk, #git or #none"

	self _resolvedProject repoType: aSymbol
]

{ #category : 'transitions' }
RwDefinedProject >> resolve [
	^ RwResolvedProject fromDefinedProject: self
]

{ #category : 'transitions' }
RwDefinedProject >> resolveStrict [
	^ RwResolvedProject fromStrictDefinedProject: self
]

{ #category : 'accessing' }
RwDefinedProject >> revision: aString [
	self _loadSpecification revision: aString
]

{ #category : 'accessing' }
RwDefinedProject >> specName: aString [
	self _loadSpecification specName: aString
]

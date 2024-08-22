"
Fundamental concepts:

	read			(disk -> def)
	write			(def -> disk)

	install			(def -> image)
	derive			(image -> def)

	load			(disk ->  image)
	commit			(image -> disk)

	project			manages collections of packages
	package			manages collections of definitions
	definitions	manages properties of classes and methods
	repository		manages disk representation of defintions
"
Class {
	#name : 'RwAbstractTool',
	#superclass : 'Object',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'commands' }
RwAbstractTool class >> git [

	^ RwGitTool new
]

{ #category : 'class lookup' }
RwAbstractTool >> definitionsForClassExtensionNamed: className ifFound: foundBlock ifAbsent: absentBlock [
	"validate loaded class extension registry structure"

	^ Rowan image
		loadedClassExtensionsNamed: className
		ifFound: [ :loadedClassExtensionSet | 
			loadedClassExtensionSet
				do: [ :loadedClassExtension | 
					| loadedPackage projectDef packageDef |
					loadedPackage := loadedClassExtension loadedPackage.
					projectDef := loadedPackage loadedProject asDefinition.
					packageDef := projectDef packageNamed: loadedPackage name.
					(packageDef classExtensions at: loadedClassExtension name ifAbsent: [  ])
						ifNotNil: [ :classExtensionDef | foundBlock value: classExtensionDef value: packageDef value: projectDef ] ].
			^ self ]
		ifAbsent: absentBlock
]

{ #category : 'class lookup' }
RwAbstractTool >> definitionsForClassNamed: className ifFound: foundBlock ifAbsent: absentBlock [

	Rowan image
		loadedClassNamed: className
		ifFound: [ :loadedClass | 
			| loadedPackage projectDef packageDef classDef |
			loadedPackage := loadedClass loadedPackage.
			projectDef := loadedPackage loadedProject asDefinition.
			packageDef := projectDef packageNamed: loadedPackage name.
			classDef := packageDef classDefinitions at: loadedClass name.
			^ foundBlock value: classDef value: packageDef value: projectDef ]
		ifAbsent: absentBlock
]

{ #category : 'class lookup' }
RwAbstractTool >> definitionsForMethod: selector inClassNamed: className isMeta: isMeta ifFound: foundBlock ifAbsent: absentBlock [

	^ Rowan image
		loadedMethod: selector
		inClassNamed: className
		isMeta: isMeta
		ifFound: [ :loadedMethod | 
			| loadedPackage loadedClass projectDef packageDef classDef classExtensionDef |
			loadedPackage := loadedMethod loadedPackage.
			loadedClass := loadedMethod loadedClass.
			projectDef := loadedPackage loadedProject asDefinition.
			packageDef := projectDef packageNamed: loadedPackage name.
			loadedClass isLoadedClass
				ifTrue: [ classDef := packageDef classDefinitions at: loadedClass name ]
				ifFalse: [ classExtensionDef := packageDef classExtensions at: loadedClass name ].
			foundBlock
				value: loadedMethod asDefinition
				value: classDef
				value: classExtensionDef
				value: packageDef
				value: projectDef.
			^ self ]
		ifAbsent: absentBlock
]

class browsing
renameClassNamed: className to: newName

	"During renameClassNamed:to: a class is created with the new name and all methods 
		are copied from the old class to the new class. If there are subclasses of the old class,
		the subclasses are moved under the new class, then the old class is removed. It is
		recommended that before renaming a class, you should find references to the class 
		and be prepared to edit the methods once the rename is complete.

	Worry about the fact that any references to the original class that may be in methods in 
		the class itself or in subclass methods will error out, if recompiled 
	"

	"anser the new copy of the class"

	| projectSetDefinition loadedClass loadedPackage loadedProject projectDef packageDef |
"1. find references to the original class [not yet implemented]"

"2. copy class to renamed class"
	projectSetDefinition := self _copyClassDefinitionNamed: className to: newName.

	loadedClass := self 
		_loadedClassNamed: className 
		ifAbsent: [  self error: 'No loaded class named: ', className printString , ' found.' ].

"3. remove original class definition"
	loadedPackage := loadedClass loadedPackage.
	loadedProject := loadedPackage loadedProject.
	projectDef := projectSetDefinition projectNamed: loadedProject name ifAbsent: [ self error: 'No loaded project named: ', loadedProject printString , ' found.'].
	packageDef := projectDef packageNamed: loadedPackage name.

	packageDef removeClassNamed: className.

"4. change superclass for all subclasses of original class to renamed class"
	(Rowan globalNamed: className) subclasses do: [:subclass |
		| subclassName classDef  |
		subclassName := subclass name asString.
		loadedClass := self 
			_loadedClassNamed: subclassName
			ifAbsent: [  self error: 'No loaded class named: ', subclassName printString , ' found.' ].

		loadedPackage := loadedClass loadedPackage.
		loadedProject := loadedPackage loadedProject.
		projectDef := projectSetDefinition 
			projectNamed: loadedProject name 
			ifAbsent: [ 
				| pDef |
				pDef := loadedPackage loadedProject asDefinition.
				projectSetDefinition addProject: pDef.
				pDef ].
		packageDef := projectDef packageNamed: loadedPackage name.
		classDef := packageDef classDefinitions at: subclassName.
		classDef superclassName: newName ].

"load projectSetDefinition & do rename"
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	^ Rowan globalNamed: newName
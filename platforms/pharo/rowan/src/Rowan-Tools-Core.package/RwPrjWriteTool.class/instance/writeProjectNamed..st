smalltalk api
writeProjectNamed: projectName

	| loadedProject projectDefinition |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	projectDefinition := loadedProject asDefinition.
	self writeProjectDefinition: projectDefinition.
	"loaded project and loaded packages written to disk - mark them not dirty"
	loadedProject markNotDirty.
	loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ].
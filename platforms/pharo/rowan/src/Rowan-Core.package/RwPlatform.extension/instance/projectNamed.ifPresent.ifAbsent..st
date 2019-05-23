*rowan-core
projectNamed: aName ifPresent: presentBlock ifAbsent: absentBlock

	"Lookup a loaded project with the given name, if found evaluate the <presentBlock>, if not evaluate the <absentBlock.."

	^ Rowan image
		loadedProjectNamed: aName
		ifPresent: [:loadedProject |  presentBlock cull: (RwProject newNamed: aName) ]
		ifAbsent: absentBlock
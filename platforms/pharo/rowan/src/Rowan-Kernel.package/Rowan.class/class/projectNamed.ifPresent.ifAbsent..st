public
projectNamed: aName ifPresent: presentBlock ifAbsent: absentBlock

	"Lookup a project with the given name, if found evaluate the <presentBlock>, if not evaluate the <absentBlock.."

	^ self platform 
		projectNamed: aName 
			ifPresent: presentBlock 
			ifAbsent: absentBlock
accessing
_projectDefinitionSourceKey
	"The value of the property key indicates which source the project definition was derived from.
		Currently used when deciding whether to change the loaded commit id, during a load ... 
		if the load is derived from a loaded project (RwLoadedProject class _projectDefinitionSourceValue), 
		then it isn't necessary to update the loaded commit id."

	^ '_Project_Definition_Source'
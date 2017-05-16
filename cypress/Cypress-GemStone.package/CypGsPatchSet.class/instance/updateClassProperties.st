"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
updateClassProperties
	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

"*** Should also update properties of added and versioned classes here?***"
	classesWithPropertyChanges do: [:each | each halt: 'Not yet implemented.']
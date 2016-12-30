"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
updateFromClass: aClass
	"Update my properties from the class. Leave unknown properties alone, they may apply to another platform.
	Things taken from the class and its environment:
	* Symbol dictionary name
	* Class name
	* Superclass name
	* Instvar names
	* Class instvar names
	* Class variable names
	* Pool Dictionaries
	* gs Class type (pointer, indexable, byte, NSC, immediate)
	* gs Options
	* Comment
	* Category"
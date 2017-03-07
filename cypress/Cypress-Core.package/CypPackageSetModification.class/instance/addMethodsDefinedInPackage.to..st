"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
addMethodsDefinedInPackage: package to: dictionary

	package classDefinitions do: 
			[:class |
			self
				addMethodsDefinedInPackage: package
				classOrExtension: class
				to: dictionary].
	package classExtensions do: 
			[:classExtension |
			self
				addMethodsDefinedInPackage: package
				classOrExtension: classExtension
				to: dictionary]
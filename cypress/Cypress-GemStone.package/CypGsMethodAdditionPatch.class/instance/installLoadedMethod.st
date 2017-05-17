"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-installing
"
installLoadedMethod

	| loadedMethod loadedClassOrExtension |
	loadedMethod := self createLoadedMethod.
	loadedClassOrExtension := self loadedClassOrExtensionForMethod.
	loadedClassOrExtension addLoadedMethod: loadedMethod.
	loadedMethod setPropertiesTo: methodDefinition properties
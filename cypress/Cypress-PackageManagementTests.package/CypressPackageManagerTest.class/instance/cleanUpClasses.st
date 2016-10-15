"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: set up / teardown
"
cleanUpClasses

	classesToCleanUp reverseDo: [:each | each value removeKey: each key name]
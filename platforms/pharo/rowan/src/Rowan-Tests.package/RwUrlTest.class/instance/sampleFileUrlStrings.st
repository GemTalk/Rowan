private
sampleFileUrlStrings
	"file: url, pathString pairs"

	"the complicated bit is the env variable in the url"

	^ {
		{ 'file:/home/dhenrich/_home/shared/repos/rowan/sample/' . '/home/dhenrich/_home/shared/repos/rowan/sample/' }.
		{ 'file:/home/dhenrich/_home/shared/repos/rowan/sample/spec.ston' . '/home/dhenrich/_home/shared/repos/rowan/sample/spec.ston' }.
		{ 'file:$GS_HOME/shared/repos/rowan/sample/' . '$GS_HOME/shared/repos/rowan/sample/' }.
		{ 'file:$GS_HOME/shared/repos/rowan/sample/spec.ston' . '$GS_HOME/shared/repos/rowan/sample/spec.ston' }.
	}
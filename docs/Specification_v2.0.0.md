# Rowan v2.0.0
## Summary
## Introduction
## Rowan Specification
### Defining an image
The numbered items marked with `--` are obsolete and subject to a planned change.
1. A Smalltalk image is composed of a collection of configurations.
2. A configuration is a named collection of configurations and packages.
3. A package is a named collection of class definitions, class extensions and method definitions.
4. A package may only be present in one loaded configuration.
4. A class definition defines the attributes of a class. The class so defined is considered to be defined in the package. A class definition includes a list of class and instance methods for the class, that are also considered to be defined in the package.
4. A class extension defines a list of class and instance side methods for a class that is not defined in the package.
5. A method definition defines the source code and protocol of a method.
3. A configuration is loaded from a project.
4. --A project is a named collection of paths to directory structure that may be physical disk, within a git commit or in a memory filesystem, etc.
5. --There are three separate root directories in a project: specs, configurations, and src. The specs directory contains a collection of load specifications stored as STON files (.ston extension). The configurations directory contains a collection of top-level and nested configurations stored as STON files. The src directory contains a collection of packages stored using Filetree or Tonel disk package formats.
6. All packages referenced in a configuration will be found in the src directory of the project associated with the configuration.
7. A kernel configuration represents the collection of classes and methods that are part of the image provided by a vendor. A kernel configuration is versioned by the image vendor. Different versions of the kernel may be loaded into the image as long as the primitive set is compatible.
8. The version of the kernel configuration is used for conditionally including packages in a loaded configuration.
9. A kernel configuration may be implied as long as the version of the kernel configuration is defined.
6. A top-level configuration may be loaded into the image by Rowan. A top-level configuration represents a complete unit of functionality that depends upon the classes and methods defined in the kernel configuration.
7. A kernel configurations is a kind of top-level configuration.
7. A nested configuration may not be directly loaded into the image by Rowan. A nested configuration is referenced directly or indiretly from a loadable, top-level configuration.
8. A nested configuration represents a pluggable unit of functionality that depends upon classes and methods defined via the top-level configuration that include it.
8. When a configuration is loaded, the commit id of the project from which it is loaded (SHA in git) is recorded with the configuration.
9. When a top-level configuration is loaded, all of the configurations referenced by the configuration are reloaded.
10. Configration reference paths may be recursive.
11. --A project reference configuration is a specialized top-level configuration. 

### Examples
#### Create and Load Project ... no disk artifacts
##### Create and load a v1.2.x in memory project
```smalltalk

	| projectName packageName projectDefinition spec classDefinition |
"initialize"
	projectName := 'Project'.
	packageName := 'Project-Core'.

"create projects"
	spec := RwSimpleProjectSpecification new
		specName: projectName;
		repoSpec: RwDiskRepositorySpecification new;
		yourself.
	projectDefinition := (RwProjectDefinition newForSpecification: spec)
		addPackageNamed: packageName;
		yourself.

	classDefinition := (RwClassDefinition
		newForClassNamed: 'ProjectClass'
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: 'category'
			comment: 'comment'
			pools: #()
			type: 'normal')
		yourself.

	(projectDefinition packageNamed: packageName)
		addClassDefinition: classDefinition.

"load"
	Rowan projectTools load loadProjectDefinition: projectDefinition.
```
Drawback of this code and [fundamental issue with v1.x][1], is that there is no explicit configration used or created.
I also don't like fact that we have some early binding for the repoSpec of the project.
##### Create and load a v2.0.0 in memory configuration
```smalltalk
	| configurationName projectName packageName |
"initialize"
	configurationName := 'Core'.
	projectName := 'Project'.
	packageName := 'Project-Core'.

"create configuration"
	configurationDefinition := (RwConfigurationDefinition 
		newTopLevelNamed: configurationName)
			addPackageNamed: packageName
				onCondition: 'gemstone';
			yourself.

	(configurationDefinition packageNamed: packageName)
		addClassDefinition: 
			(RwClassDefinition 
				newForClassNamed: 'ProjectClass'
				superclassNamed: 'Object').

"load"
	configurationDefinition load
```
##### Attach a git-based repository to a v2.0.0 project. Create/write/commit
```smalltalk
	| projectName projectDefinition |
"lookup"
	projectName := 'Project'.
	projectDefinition := Rowan projectDefinitionForProjectNamed: projectName.

"attach repository"
	projectDefinition 
		attachGitRepository: Rowan projectsHome / projectName
			configsPath:  'rowan/configs'
			srcPath: 'rowan/src'
			format: 'tonel';
			yourself.

"create/write/commit"
	projectDefinition
		createRepository;
		writeRepository;
		commitRepository: 'first commit'
```
[1]: https://github.com/GemTalk/Rowan/issues/376


# Rowan v2.0.0
## Summary
## Introduction
## Rowan Specification
### Defining an image
The numbered items marked with `--` are obsolete and subject to a planned change.
1. A Smalltalk image is composed of a collection of loaded projects.
1. A loaded project is a collection of loaded components defined by a load specification.
1. A load specification is a named collection of project properties:
  1. defining the structure of the project on disk:
    1. `specs` directory where load specifications are stored.
    2. `components` directory where component definitions are stored.
    3. `projects` directory where required project specifications are stored.
    4. `packages` directory where packages are stored.
  2. the list of componentNames and groups to be loaded when the specification is resolved.
  3. the url and type of the repository technology to be used for storing the project artifacts on disk.
2. A component is a named collection of components and packages.
3. A package is a named collection of class definitions, class extensions and method definitions.
4. A package may only be present in one loaded component.
4. A class definition defines the attributes of a class. The class so defined is considered to be defined in the package. A class definition includes a list of class and instance methods for the class, that are also considered to be defined in the package.
4. A class extension defines a list of class and instance side methods for a class that is not defined in the package.
5. A method definition defines the source code and protocol of a method.

3. A component is loaded from a project.
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

### v2.0.0 Examples
1. [Create and Load ComponentDefinitions](#create-and-load-componentdefinitions)
2. [Attach New Git-based Repository to Loaded Project - create/write/commit repository](#attach-new-git-based-repository-to-loaded-project)
2. [Attach Existing Git-based Repository to Loaded Project](#attach-existing-git-based-repository-to-loaded-project)
3. [Clone and Load GitHub Project using Load Spec Url](#clone-and-load-github-project-using-load-spec-url)

#### Create and Load ComponentDefinitions
```smalltalk
| repo |
repo := RwMemoryRepositoryDefinition
	newNamed: 'Project Repository'
		repositoryRoot: '/repos/Example'
		specsPath: 'rowan/specs'
		configsPath: 'rowan/configs'
		srcPath: 'rowan/src'.

"create component"
componentDef := RwComponentDefinition newNamed: 'Example'.
(componentDef addPackageNamed: 'Example-Core')
		addClassDefinition: 
			((RwClassDefinition 
				newForClassNamed: 'ExampleCoreClass'
				super: 'Object'
				category: 'Example-Common')
					addInstanceMethodDefinition: 
						(RwMethodDefinition newForSource: 'bar ^ 1' protocol: 'accessing');
					addClassMethodDefinition: 
						(RwMethodDefinition newForSource: 'bar ^ 2' protocol: 'accessing');
					yourself).
```

#### Create and Load in memory Configuration
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
#### Attach New Git-based Repository to Loaded Project
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
		commitToRepository: 'first commit'
```
#### Attach Existing Git-based Repository to Loaded Project
```smalltalk
	| projectName projectDefinition |
"lookup"
	projectName := 'Project'.
	projectDefinition := Rowan projectDefinitionForProjectNamed: projectName.

"attach existing repository"
	projectDefinition 
		attachGitRepository: Rowan projectsHome / 'temp' / projectName.

"load configuration from new location"
	projectDefinition loadedConfigurationDefinition load
```
#### Clone and Load GitHub Project using Load Spec Url
Sample load specification:
```ston
RwSimpleProjectSpecification {
	#specName : 'RowanSample4',
	#projectUrl : 'https://github.com/dalehenrich/RowanSample4',
	#repoSpec : RwGitRepositorySpecification {
		#committish : 'master',
		#committishType : 'branch'
	},
	#configsPath : 'rowan/configs',
	#repoPath : 'rowan/src',
	#specsPath : 'rowan/specs',
	#defaultConfigurationNames : [ 'Load' ],
	#comment : 'Sample project to be used as an example project when first getting started with Jadeite and Rowan.'
}
```
Script to load the default configuration using a spec url:
```smalltalk
	| specUrl projectDefinition projectRootPath |
"init"
	projectRootPath := Rowan projectsHome / projectName.
	specUrl :=  'file:' ,  (projectRootPath / 'samples/RowanSample4_core.ston') pathString.

"create configuration"
	projectDefinition := RwProjectDefinition 
		newFromSpecUrl: specUrl
		projectRootPath: projectRootPath.
	projectDefinition cloneRepository.	"clone perfomed only if the repo not present on disk"

"load"
	projectDefinition defaultConfigurationDefinition load
```
Script to load a non-default configuration using a spec url:
```smalltalk
	| specUrl projectDefinition projectRootPath |
"init"
	projectRootPath := Rowan projectsHome / projectName.
	specUrl :=  'file:' ,  (projectRootPath / 'rowan/specs/RowanSample4_core.ston') pathString.

"create configuration"
	projectDefinition := RwProjectDefinition 
		newFromSpecUrl: specUrl
		projectRootPath: projectRootPath.
	projectDefinition cloneRepository.	"clone perfomed only if the repo not present on disk"

"load"
	(projectDefinition configurationNamed: 'Load_core') load

```

## Components

### Components
A **component** is the loadable unit in Rowan.
A **component** specifies a complete and independent unit of functionality.
A **component** has a name and is located on disk in the *components directory*, specified by the *components path* of the *project specification*.

A **component** is composed of:
- A list of *dependent project names*. When a **component** is loaded, the *dependent projects* are loaded as well.
- A *preload doit name*. 
The *preload doit* is performed before the **component** itself is loaded.
- A *postload doit name*. 
The *postload doit* is performed after the **component** itself is loaded.
- A list of *package names*. 
When a **component** is loaded, the listed packages are loaded.
- A list of *component names*. 
When a **component** is loaded, the packages listed by the referenced components are loaded.
The list may reference **components** or **subcomponents** by name and recursive references (direct or indirect) are allowed.

Here's a sample **RwComponent** ston file with two **subcomponents**:
```ston
RwComponent {
	#name : 'Core',
	#projectNames : [ ],
	#componentNames : [
		'conditions/Core',
		'tests/Tests'
	],
	#packageNames : [
		'RowanSample9-Core'
	],
	#comment : 'Primary component used for loading the core classes.'
}
```

#### Components and Tests
A **component** should have an associated set of *tests* that validates the public API for the component.
As a loadable unit there is an implied contract between the developers and users of the component that the public API implemented by the classes and methods loaded by the component and the test* are expected to validate that contract.

The tests may be defined by packages named in the component itself, or they may be defined by packages named in subcomponents of the component. It is recommended that test* be defined in one or more `Tests` subcomponents that are conditional on a `test` *conditional attribute*. In this way end users can control whether or not they want tests loaded into an image.
### Subcomponents
A **subcomponent** has a name and is located on disk in the *components directory* or a subdirectory of the *components directory*. If the **subcomponent** is located in a subdirectory, then the name of the **subcomponent** must be the path to the **subcomponent** relative to the *components directory*.

Similar to the **component**, a **subcomponent** is composed of:
- A list of *dependent project names*.
- A *condition*.
- A *preload doit name*. 
- A *postload doit name*. 
- A list of *package names*. 
- A list of *component names*.

#### Conditional Subcomponents
A **subcomponent** has a *condition* associated with it. 
A *condition* is a single *conditional attribute*.

If a **subcomponent** is referenced either directly or indirectly from a **component** that is slated to be loaded (i.e., listed in the *component names* of the *load specification* used to *resolve* the *project*), the **subcomponent** *condition* is matched against the *platform conditional attributes* and if there is a match, then the **subcomponent** will be `active` and it's packages, components, doits and projects will participate in the load.

A **subcomponent** that references another **subcomponent** via its list of *component names* is an effective `AND` of the two *conditions* for the **referenced subcomponent**

Here's a sample of a **RwSubcomponent** ston file with a **subcomponens**, a *package*, and a *condition* of 'alt1':
```ston
RwSubcomponent {
	#name : 'tests/alt1/Tests',
	#condition : 'alt1',
	#projectNames : [ ],
	#componentNames : [
		'tests/alt1/platform/Tests'
	],
	#packageNames : [
		'RowanSample9-Tests-Alt1'
	],
	#comment : ''
}
```
#### Subcomponents and Tests
A **subcomponent** may only be loaded via a direct or indirect reference from a **component**.
There is no implied API for a **subcomponent**, so no associated *test* are expected.

### PlatformComponents
A **platformComponent** is a special case of **subcomponent**, where the *condition* is a collection of *conditional attributes* and/or *platform version patterns*.
Each of the items in the list are effectively ORed together.
The use case for ORing is when you have a set of packages that are conditional on a set of platforms. For example here are several **platformComponent** *conditions*:
1. `#( 'gemstone' 'pharo' )`
2. `#( 'gs3.2.1.x' 'pharo7.x' )`
3. `#( 'gs3.[1-]' 'pharo7.[1-5]' )`

If you are familiar with [Metacello][1], the the *conditions* in items 1 and 2 should be familiar to you.

The `#( 'gemstone' 'pharo' )` *condition* in item 1 is a case where no pattern matching is needed, if the *platform conditional attributes list* contains wither 'gemstone' or 'pharo' the *condition* is matched and the **subcomponent** is active. 
For these *exact match* cases, it isn't necessary that the attributes in the pattern list be the name of a platform.
However, items 2 and 3, involves matching platform specific version strings.

The `#( 'gs3.2.1.x' 'pharo7.x' )` *condition* where the `x` in each pattern may be replaced with a continuation of the version string.
The pattern  'gs3.2.1.x' will match all of the following gemstone version strings: '3.2.1.0', '3.2.1.2', '3.2.1.2.5', '3.2.1.10' and not match the following gemstone version strings: '3.2.1', '3.2.2'.

From experience with Metacello, t is clear that a pattern is also needed that allows for matching version ranges and the example in item 3 illustrates the version range pattern.
The pattern 'gs3.[1-]' will do an open ended match ot gemstone version strings from '3.1.0' up to, but not including version '4.0'.
The pattern 'pharo7.[1-5]' will match pharo version strings in a closed range from '7.1' through '7.5'.

Here's an example of a **RwPlatformSubcomponent** with a *package* and a *condition* of 'platformA':
```ston
RwPlatformSubcomponent {
	#name : 'tests/alt1/platform/platformA/Tests',
	#condition : [
		'platformA'
	],
	#projectNames : [ ],
	#componentNames : [ ],
	#packageNames : [
		'RowanSample9-Tests-PlatformA'
	],
	#comment : ''
}
```

### Component directory structure
As mentioned earlier, it is expected that **components** be located in the *components directory*. It then follows that **subcomponents** should be located in subdirectories, to segregate the **subcomponents** and **components**.
Since **subcomponents** have a *condition* it makes sense to organize **subcomponents** by the value of their *conditions*
Here's a picture of an example component directory structure:
<img src="https://github.com/GemTalk/Rowan/blob/issue_660/docs/component_directory_structure.png" alt="component directory structure" width="250px" >
### CategoryComponent and CategorySubcomponent
The **categoryComponent** and **categorySubcomponent** are used to provide code organization within a package/class browser.
The current Jadeite Project browser
displays the list of projects, then list of packages in selected project, then list of classes in selected package, etc: 
<img src="https://github.com/GemTalk/Rowan/blob/issue_660/docs/Jadeite_project_browser.png" alt="Jadite Project Browser" width="1000px" >
When category components are specified in a project, the packages pane is replaced by a hierarchical category component view composed of the list of the names of the **categoryComponents** directly referenced from the loaded **components**.
The package pane has a *package* tab and a *category* tab. 
When the *package* tab is selected, the list of packages in the project will be displayed.
When the *category* tab is selected, the list of **categoryComponents** referenced by the loaded **components** will be displayed.

Selecting a **categoryComponent** or **categorySubcomponent** will cause list of classes referenced by the component to be displayed in the classes pane.
Expanding the selected component will display the list of **categorySubcomponents** in the selected component and so on. 

A **categoryComponents** can only be referenced by **components**.
A **categoryComponents** can reference only **categorySubcomponents**.

A **categorySubcomponents** can only be referenced by **categoryComponents** or **categorySubcomponents**.
A **categorySubcomponents** may reference  **categorySubcomponents**, **components**, and **subcomponents**.

**categoryComponent** and **categorySubcomponent** are not conditional and do not participate in loading decisions.
If a component referenced by a **categoryComponent** or **categorySubcomponent** is not loaded, then the component is ignored for display purposes.
 
### Examples
#### Query for list of ALL components, packages and conditions
Returns an array of 4 dictionaries:
1. **components** is a map of all components on disk indexed by component name.
2. **packages** is a map all components on disk indexed by package name.
3. **conditions** is a map of all components and packages on disk indexed by condition.
4. **platformConditions** is a map all components and packages on disk indexed by platform condition array.
```smalltalk
|  project components packages conditions platformConditions notFoundBlock |
project := Rowan projectNamed: 'Rowan'.
components := Dictionary new.
packages := Dictionary new.
conditions := Dictionary new.
platformConditions := Dictionary new.
notFoundBlock := [:missingComponentName |
		| missingComponent |
		missingComponent := RwAbstractComponent 
			fromComponentsDirectory: project componentsRoot
			named: missingComponentName.
		components at: missingComponentName put: missingComponent packageNames.
		missingComponent ].
project _loadedProject 
	allComponentsIn: 'Rowan' 
	matchBlock: [:ignored | true ] 
	notFound: notFoundBlock
	do: [:component | 
		| condition |
		condition := component condition.
		condition _isArray
			ifTrue: [ 
				| dict |
				dict := platformConditions at: condition ifAbsentPut:  [ Dictionary new ].
				(dict at: 'packages' ifAbsentPut: [ Set new])
					addAll: component packageNames.
				(dict at: 'components' ifAbsentPut: [ IdentitySet new])
					add: component ]
			ifFalse: [ 
				| dict |
				dict := conditions at: condition ifAbsentPut:  [ Dictionary new ].
				(dict at: 'packages' ifAbsentPut: [ Set new])
					addAll: component packageNames.
				(dict at: 'components' ifAbsentPut: [ IdentitySet new])
					add: component.
 				(dict at: 'allPackageNames' ifAbsentPut: [ Set new])
					addAll: 
						(project _loadedProject
							allPackageNamesIn: component name 
							matchBlock: [:ignored | true ] 
							notFound: notFoundBlock) ].
		components at: component name put: component.
		component packageNames do: [:packageName |
			packages at: packageName ifPresent: [ self halt: 'package duplication'].
			packages at: packageName put: component ] ].
{ components . packages . conditions . platformConditions }
```
[1]: https://github.com/dalehenrich/metacello-work

package com.logicaldoc.core.automation;

import java.lang.reflect.InvocationTargetException;

/**
 * Utility functions for manipulating classes and resources.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
@AutomationDictionary
public class ClassTool {

	/**
	 * Creates a new instance of a class whose constructor does not have
	 * parameters
	 * 
	 * @param className name of the class, if the class is inside the package
	 *        com.logicaldoc you can just avoid the prefix, e.g.: for
	 *        com.logicaldoc.stamp.automation.StampTool you can simply use
	 *        stamp.automation.StampTool)
	 * 
	 * @return the instantiated object
	 * 
	 * @throws InstantiationException the class does not have a constructor
	 *         without parameters
	 * @throws IllegalAccessException the class or the constructor is private or
	 *         not visible
	 * @throws ClassNotFoundException you specified an unexisting class
	 * @throws SecurityException the specified class cannot be instantiated due
	 *         to security constraints
	 * @throws NoSuchMethodException no public constructor without parameters
	 * @throws InvocationTargetException the constructor raised an exception
	 * @throws IllegalArgumentException no public constructor without parameters
	 */
	public Object newInstance(String className)
			throws InstantiationException, IllegalAccessException, ClassNotFoundException, IllegalArgumentException,
			InvocationTargetException, NoSuchMethodException, SecurityException {
		try {
			return Class.forName(className).getDeclaredConstructor().newInstance();
		} catch (Throwable t) {
			// The classname as is was not found, so try to prefix with our root
			// package
			return Class.forName("com.logicaldoc." + className).getDeclaredConstructor().newInstance();
		}
	}
}
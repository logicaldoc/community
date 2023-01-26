package com.logicaldoc.core.automation;

import java.lang.reflect.InvocationTargetException;

import org.apache.commons.lang3.StringUtils;

/**
 * Utility functions for manipulating classes and resources and for other
 * programming language aspects
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
		if (className.equals("java.lang.Runtime"))
			throw new SecurityException("Class java.lang.Runtime is forbidden and cannot be instanciated");

		try {
			return Class.forName(className).getDeclaredConstructor().newInstance();
		} catch (Throwable t) {
			// The classname as is was not found, so try to prefix with our root
			// package
			return Class.forName("com.logicaldoc." + className).getDeclaredConstructor().newInstance();
		}
	}

	/**
	 * Checks if a given instance is null
	 * 
	 * @param instance the object to evaluate
	 * 
	 * @return true only if the passed instance is null
	 */
	public boolean isNull(Object instance) {
		return instance == null;
	}
	
	/**
	 * Checks if a given string is null
	 * 
	 * @param str the string to evaluate
	 * 
	 * @return true only if the passed string is null
	 */
	public boolean isEmptyStrin(String str) {
		return StringUtils.isEmpty(str);
	}
}
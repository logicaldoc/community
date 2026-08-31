package com.logicaldoc.core.automation;

import java.lang.reflect.InvocationTargetException;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility functions for manipulating classes and resources and for other
 * programming language aspects
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
@AutomationDictionary
public class ClassTool {

	private static final Logger log = LoggerFactory.getLogger(ClassTool.class);

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
			throw new SecurityException("Class java.lang.Runtime is forbidden and cannot be instantiated");

		try {
			return Class.forName(className).getDeclaredConstructor().newInstance();
		} catch (Exception t) {
			// The classname as is was not found, so try to prefix with our root
			// package
			return Class.forName("com.logicaldoc." + className).getDeclaredConstructor().newInstance();
		}
	}

	/**
	 * Retrieves the class specification
	 * 
	 * @param className name of the class, if the class is inside the package
	 *        com.logicaldoc you can just avoid the prefix, e.g.: for
	 *        com.logicaldoc.stamp.automation.StampTool you can simply use
	 *        stamp.automation.StampTool)
	 * 
	 * @return the class object
	 * 
	 * @throws ClassNotFoundException you specified an unexisting class
	 */
	public Class<?> forName(String className) throws ClassNotFoundException {
		if (className.equals("java.lang.Runtime"))
			throw new SecurityException("Class java.lang.Runtime is forbidden and cannot be instantiated");

		try {
			return Class.forName(className);
		} catch (Exception t) {
			// The classname as is was not found, so try to prefix with our root
			// package
			return Class.forName("com.logicaldoc." + className);
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
	 * Checks if a given string is null or empty
	 * 
	 * @param str the string to evaluate
	 * 
	 * @return true only if the passed string is null or empty
	 */
	public boolean isEmpty(String str) {
		return StringUtils.isEmpty(str);
	}

	/**
	 * Makes a copy(clone) of a given object instance.<br>
	 * <p>
	 * <b>Attention:</b> for security reasons it will not be used the cloned()
	 * method of the given objet but a new instance of the given object's class
	 * is invoked by passing the same instance. As a result they may be copied
	 * only those objects that define a public construction that accepts the
	 * same class as single argument.
	 * </p>
	 * 
	 * @param instance the object to clone
	 * 
	 * @return the cloned object
	 */
	public Object copy(Object instance) {
		try {
			return instance.getClass().getDeclaredConstructor(instance.getClass()).newInstance(instance);
		} catch (Exception e) {
			log.error("Cannot make a copy of {}", instance);
			log.error(e.getMessage(), e);
			return null;
		}
	}
}
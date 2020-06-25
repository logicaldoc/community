package com.logicaldoc.core.automation;

import static java.lang.annotation.ElementType.TYPE;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a class to be added to the automation dictionary
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(TYPE)
public @interface AutomationDictionary {
	String key() default "";
}

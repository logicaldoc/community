package com.logicaldoc.webservice.doc;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;


/**
 * An annotation to document the web services.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
@Retention(RUNTIME)
@Target({ FIELD, METHOD, PARAMETER, TYPE })
public @interface WSDoc {

	boolean required() default true;

	boolean documented() default true;

	String description() default "";
}
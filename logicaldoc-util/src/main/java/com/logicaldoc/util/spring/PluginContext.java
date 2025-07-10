package com.logicaldoc.util.spring;

import static java.lang.annotation.ElementType.TYPE;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.ApplicationContext;

/**
 * Marks a class to be added to the {@link ApplicationContext}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(TYPE)
public @interface PluginContext {
}
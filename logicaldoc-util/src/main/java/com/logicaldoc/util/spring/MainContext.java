package com.logicaldoc.util.spring;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.ImportResource;

/**
 * Main context with proper annotations for starting up the whole application
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
@ComponentScan(basePackages = "com.logicaldoc", useDefaultFilters = false, includeFilters = @ComponentScan.Filter(type = FilterType.ANNOTATION, value = PluginContext.class))
@ImportResource("classpath:context.xml")
public class MainContext {
	// Empty class
}
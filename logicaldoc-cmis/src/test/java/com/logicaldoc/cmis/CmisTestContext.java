package com.logicaldoc.cmis;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.context.annotation.ImportResource;

import com.logicaldoc.util.spring.PluginContext;

/**
 * Main ApplicationContext for testing this module
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.1
 */
@ComponentScan(basePackages = {"com.logicaldoc.core", "com.logicaldoc.cmis"}, useDefaultFilters = false, includeFilters = @ComponentScan.Filter(type = FilterType.ANNOTATION, value = PluginContext.class))
@ImportResource("classpath:contexttest.xml")
public class CmisTestContext {
	// Main ApplicationContext for testing this module
}
package com.logicaldoc.core;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ImportResource;

import com.logicaldoc.util.spring.PluginContext;

/**
 * Context of the core plugin
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.1
 */
@PluginContext
@ComponentScan("com.logicaldoc.core")
@ImportResource("classpath:context/context-core.xml")
public class CoreContext {
	// Context of the core plugin
}
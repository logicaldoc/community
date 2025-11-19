package com.logicaldoc.webservicedoc;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ImportResource;

import com.logicaldoc.util.spring.PluginContext;

/**
 * Context of the webdav plugin
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.1
 */
@PluginContext
@ComponentScan("com.logicaldoc.webservicedoc")
@ImportResource("classpath:context/context-webservicedoc.xml")
public class WebservicedocContext {
	// Context of the webdav plugin
}
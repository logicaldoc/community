package com.logicaldoc.webservice;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ImportResource;

import com.logicaldoc.util.spring.PluginContext;

/**
 * Context of the webserivce plugin
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.1
 */
@PluginContext
@ComponentScan("com.logicaldoc.webservice")
@ImportResource("classpath:context/context-webservice.xml")
public class WebserviceContext {
	// Context of the webserivce plugin
}
package com.logicaldoc.webdav;

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
@ComponentScan("com.logicaldoc.webdav")
@ImportResource("classpath:context/context-webdav.xml")
public class WebdavContext {
	// Context of the webdav plugin
}
package com.logicaldoc.cmis;

import org.springframework.context.annotation.ComponentScan;

import com.logicaldoc.util.spring.PluginContext;

/**
 * Main configuration for the cmis plugin
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.1
 */
@PluginContext
@ComponentScan("com.logicaldoc.cmis")
public class CmisContext {

}
package com.logicaldoc.dropbox;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ImportResource;

import com.logicaldoc.util.spring.PluginContext;

/**
 * Main configuration for the dropbox plugin
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.1
 */
@PluginContext
@ComponentScan("com.logicaldoc.dropbox")
public class DropboxContext {

}
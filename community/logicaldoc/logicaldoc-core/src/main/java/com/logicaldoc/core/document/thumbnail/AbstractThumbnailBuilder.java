package com.logicaldoc.core.document.thumbnail;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Main starting point for all thumbnail builders
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8.1
 * 
 */
public abstract class AbstractThumbnailBuilder implements ThumbnailBuilder {
	protected static Logger log = LoggerFactory.getLogger(AbstractThumbnailBuilder.class);

	protected static String CONVERT = "command.convert";
}
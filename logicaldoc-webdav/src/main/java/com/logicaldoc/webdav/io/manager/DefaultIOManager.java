package com.logicaldoc.webdav.io.manager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.DefaultIOManager}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class DefaultIOManager extends IOManagerImpl {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(DefaultIOManager.class);

	public DefaultIOManager() {
		init();
	}

	protected void init() {
		// Nothing to do
	}
}
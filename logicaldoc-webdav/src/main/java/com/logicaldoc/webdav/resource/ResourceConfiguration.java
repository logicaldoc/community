package com.logicaldoc.webdav.resource;

import java.io.Serializable;

import org.apache.jackrabbit.server.io.PropertyManager;
import org.apache.jackrabbit.server.io.PropertyManagerImpl;
import org.apache.jackrabbit.webdav.simple.DefaultItemFilter;
import org.apache.jackrabbit.webdav.simple.ItemFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webdav.io.manager.IOManager;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.ResourceConfig}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class ResourceConfiguration implements Serializable {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(ResourceConfiguration.class);

	private transient ItemFilter itemFilter;

	@jakarta.annotation.Resource(name = "IOManager")
	private transient IOManager ioManager;

	private transient PropertyManager propManager;

	public void setIOManager(IOManager ioManager) {
		this.ioManager = ioManager;
	}

	public IOManager getIOManager() {
		return ioManager;
	}

	public PropertyManager getPropertyManager() {
		if (propManager == null) {
			log.debug("ResourceConfig: missing property-manager > building default.");
			propManager = PropertyManagerImpl.getDefaultManager();
		}
		return propManager;
	}

	public ItemFilter getItemFilter() {
		if (itemFilter == null) {
			log.debug("ResourceConfig: missing resource filter > building DefaultItemFilter ");
			itemFilter = new DefaultItemFilter();
		}
		return itemFilter;
	}
}
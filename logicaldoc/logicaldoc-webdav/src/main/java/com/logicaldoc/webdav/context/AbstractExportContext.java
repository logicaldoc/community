package com.logicaldoc.webdav.context;

import org.apache.jackrabbit.server.io.IOListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webdav.resource.model.Resource;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.AbstractExportContext}
 * 
 * @author Sebastian Wenzky
 * 
 */
public abstract class AbstractExportContext implements ExportContext {

	protected static Logger log = LoggerFactory.getLogger(AbstractExportContext.class);

	private final Resource resource;

	private final boolean hasStream;

	protected boolean completed;

	@Override
	public IOListener getIOListener() {
		return null;
	}

	public AbstractExportContext(Resource resource, boolean hasStream) {
		this.resource = resource;
		this.hasStream = hasStream;
	}

	public Resource getResource() {
		return resource;
	}

	public boolean hasStream() {
		return hasStream;
	}

	public void informCompleted(boolean success) {
		completed = true;
	}

	public boolean isCompleted() {
		return completed;
	}

	protected void checkCompleted() {
		if (completed) {
			throw new IllegalStateException("ExportContext has already been finalized.");
		}
	}
}

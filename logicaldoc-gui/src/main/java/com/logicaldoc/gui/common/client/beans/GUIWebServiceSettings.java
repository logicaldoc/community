package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;

/**
 * Web Service and WebDAV Settings bean as used in the GUI.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUIWebServiceSettings implements Serializable {

	private static final long serialVersionUID = 1L;

	private String url;

	private String descriptor;

	private boolean enabled = false;

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getDescriptor() {
		return descriptor;
	}

	public void setDescriptor(String descriptor) {
		this.descriptor = descriptor;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}
}

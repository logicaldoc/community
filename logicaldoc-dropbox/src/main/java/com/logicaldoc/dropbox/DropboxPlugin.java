package com.logicaldoc.dropbox;

import com.logicaldoc.util.plugin.LogicalDOCPlugin;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Entry-point for the Dropbox plug-in
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DropboxPlugin extends LogicalDOCPlugin {

	@Override
	public void install() throws PluginException {
		super.install();

		addServlet("DropboxService", DropboxServiceImpl.class, "/frontend/dropbox");
		addServlet("DropboxData", DropboxDataServlet.class, "/data/dropbox.xml");

		setRestartRequired();
	}
}
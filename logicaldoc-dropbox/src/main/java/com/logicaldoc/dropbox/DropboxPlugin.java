package com.logicaldoc.dropbox;

import com.logicaldoc.util.plugin.LogicalDOCPlugin;

/**
 * Entry-point for the Dropbox plug-in
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DropboxPlugin extends LogicalDOCPlugin {

	@Override
	public void install() throws Exception {
		super.install();

		addServlet("DropboxService", DropboxServiceImpl.class.getName(), "/frontend/dropbox");
		addServlet("DropboxData", DropboxDataServlet.class.getName(), "/data/dropbox.xml");

		setRestartRequired();
	}
}
package com.logicaldoc.dropbox;

import java.io.File;

import com.logicaldoc.util.config.WebConfigurator;
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

		// Register the needed servlets
		File dest = new File(getPluginPath());
		dest = dest.getParentFile().getParentFile();
		WebConfigurator config = new WebConfigurator(dest.getPath() + "/web.xml");

		config.addServlet("DropboxService", DropboxServiceImpl.class.getName());
		config.addServletMapping("DropboxService", "/frontend/dropbox");
		config.writeXMLDoc();

		config.addServlet("DropboxData", DropboxDataServlet.class.getName());
		config.addServletMapping("DropboxData", "/data/dropbox.xml");
		config.writeXMLDoc();

		setRestartRequired();
	}
}
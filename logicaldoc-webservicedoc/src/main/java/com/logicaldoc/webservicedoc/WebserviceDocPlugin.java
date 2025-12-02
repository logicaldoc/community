package com.logicaldoc.webservicedoc;

import com.logicaldoc.util.plugin.LogicalDOCPlugin;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.WebserviceServlet;

/**
 * This class provides initializations needed by this plug-in
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 7.5.1
 * 
 */
public class WebserviceDocPlugin extends LogicalDOCPlugin {

	@Override
	public void install() throws PluginException {
		addServlet("CXFServlet", WebserviceServlet.class, "/app/*");
		setRestartRequired();
	}
}
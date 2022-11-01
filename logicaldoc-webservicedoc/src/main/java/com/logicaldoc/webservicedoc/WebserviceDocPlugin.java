package com.logicaldoc.webservicedoc;

import java.io.File;

import com.logicaldoc.util.config.WebConfigurator;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;
//import com.logicaldoc.webservice.WebserviceServlet;
import com.logicaldoc.util.plugin.PluginException;

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
		File dest = new File(getPluginPath());
		dest = dest.getParentFile().getParentFile();
		WebConfigurator config = new WebConfigurator(dest.getPath() + "/web.xml");
		config.addServletMapping("CXFServlet", "/app/*");
		config.writeXMLDoc();
		setRestartRequired();
	}
}
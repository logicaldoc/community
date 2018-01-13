package com.logicaldoc.webservicedoc;

import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.config.WebConfigurator;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;
//import com.logicaldoc.webservice.WebserviceServlet;

/**
 * This class provides initializations needed by this plug-in
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 7.5.1
 * 
 */
public class WebserviceDocPlugin extends LogicalDOCPlugin {
	protected static Logger log = LoggerFactory.getLogger(WebserviceDocPlugin.class);

	@Override
	protected void install() throws Exception {

		File dest = new File(getPluginPath());
		dest = dest.getParentFile().getParentFile();
		WebConfigurator config = new WebConfigurator(dest.getPath() + "/web.xml");
//		config.addServlet("CXFServlet", WebserviceServlet.class.getName());
//		config.writeXMLDoc();
		config.addServletMapping("CXFServlet", "/app/*");
		config.writeXMLDoc();
	
		setRestartRequired();
	}
}